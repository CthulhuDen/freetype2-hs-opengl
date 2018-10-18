{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Description: Rendering text with OpenGL using FreeType2
-- Module provides function to render text with OpenGL using FreeType2
module Graphics.Rendering.FreeType.OpenGL
  ( -- * Text to be rendered
    TextString
  , textPart

    -- * Rendering given text immediately
  , FontAtlas
  , newFontAtlas
  , dumpAtlas
  , render

    -- * Delated rendering of text
    -- | Can be preferred because of no need of context size until actually rendering,
    -- also it only calls opengl rendering function once for all the enqueued texts combined.
  , DelayedRenderer
  , newDelayedRenderer
  , enqueue
  , flushRender
  ) where

import Control.Monad
import Data.Char (chr)
import Data.FileEmbed
import Data.IORef
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Storable as VS
import Foreign.Ptr
import Foreign.Storable.Generic
import GHC.Generics

import qualified Graphics.Bitmap.Atlas as A
import           Graphics.Rendering.FreeType
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.GL as GLR
import qualified Graphics.GLUtil as U
import qualified Linear as L
import           Linear (V2 (..), V3 (..), V4 (..))
import           Linear.Storable ()

genTex :: (GL.BindableTextureTarget targ, GL.ParameterizedTextureTarget targ) => targ -> IO GL.TextureObject
genTex targ = do
  tex <- GL.genObjectName
  GL.textureBinding targ $= Just tex
  GL.textureWrapMode targ GL.S $= (GL.Repeated, GL.ClampToBorder)
  GL.textureWrapMode targ GL.T $= (GL.Repeated, GL.ClampToBorder)
  GL.textureFilter targ $= ((GL.Nearest, Nothing), GL.Nearest)
  pure tex

alloc2DArray :: Integral int => Int -> int -> int -> IO ()
alloc2DArray l w h = GL.texImage3D GL.Texture2DArray GL.NoProxy 0 GL.R8 texSize 0 pixData
  where
    texSize = GL.TextureSize3D (fromIntegral w) (fromIntegral h) (fromIntegral l)
    pixData = GL.PixelData GL.Red GL.UnsignedByte nullPtr

ptrInto2DLayer :: Integral int => Int -> int -> int -> Ptr a -> IO ()
ptrInto2DLayer l w h p = GL.texSubImage3D GL.Texture2DArray 0 pos size pixData
  where
    pos = GL.TexturePosition3D 0 0 (fromIntegral l)
    size = GL.TextureSize3D (fromIntegral w) (fromIntegral h) 1
    pixData = GL.PixelData GL.Red GL.UnsignedByte $ castPtr p

data TextCharacter = TextCharacter
  { tcChar  :: !Char
  , tcColor :: !(V4 GL.GLfloat)
  , tcSize  :: !PixelSizes
  } deriving (Generic)
instance GStorable TextCharacter

-- | Text to be rendered. Contains textual information as well as color/size for each character
type TextString = [TextCharacter]

-- | Create combinable ('Monoid') part of the textual string to be rendered.
textPart :: V4 GL.GLfloat -> PixelSizes -> String -> TextString
textPart tcColor tcSize = fmap (\tcChar -> TextCharacter {..})

data FontAtlasDumper = FontAtlasDumper
  { fadVao    :: !GL.VertexArrayObject
  , fadVbo :: !GL.BufferObject
  , fadProg   :: !U.ShaderProgram
  }

data FARChar = FARChar
  { fcPos      :: {-# UNPACK #-} !(V2 GL.GLfloat)
  , fcTexCoord :: {-# UNPACK #-} !(V3 GL.GLfloat)
  , fcSize     :: {-# UNPACK #-} !(V2 GL.GLfloat)
  , fcColor    :: {-# UNPACK #-} !(V4 GL.GLfloat)
  } deriving (Show, Generic)
instance GStorable FARChar

data FontAtlasRenderer = FontAtlasRenderer
  { farVao  :: !GL.VertexArrayObject
  , farVbo  :: !GL.BufferObject
  , farProg :: !U.ShaderProgram
  }

-- | Allows rendering text in fast fashion: one OpenGL invocation for whole text being rendered.
-- Required knowing all the characters/sizes you will need to render beforehand.
--
-- Still better is to use 'DelayedRenderer' as it allows you to choose time to render
-- ALL of the queued texts at once.
data FontAtlas = FontAtlas
  { faTex      :: !GL.TextureObject
  , faLoc      :: !(HM.HashMap AtlasIx (Int, (Int, Int), CharacterMetrics))
  , faRenderer :: !FontAtlasRenderer
  , faDumper   :: !FontAtlasDumper
  }

type AtlasIx = (Char, PixelSizes)

-- | Construct new font atlas given font face and all of the sizes we might later request this atlas to render.
newFontAtlas :: FT_Face -> [PixelSizes] -> IO FontAtlas
newFontAtlas ff ss = do
  (atlas, faLoc) <- makeAtlas
  faTex <- genTex GL.Texture2DArray <* alloc2DArray (A.countPages atlas) atlasPageWidth atlasPageHeight
  void $ A.withPages atlas $ \i (A.Size w h) -> ptrInto2DLayer i w h

  (faRenderer, faDumper) <- (,) <$> makeRenderer <*> makeDumper

  pure FontAtlas {..}

  where
    farCharSize = fromIntegral $ sizeOf (undefined :: FARChar)
    fcPosOffset = U.offset0
    fcTexOffset = fcPosOffset `plusPtr` sizeOf (undefined :: V2 GL.GLfloat)
    fcSizeOffset = fcTexOffset `plusPtr` sizeOf (undefined :: V3 GL.GLfloat)
    fcColorOffset = fcSizeOffset `plusPtr` sizeOf (undefined :: V2 GL.GLfloat)

    makeRenderer = do
      farProg <- U.simpleShaderProgramBS $(embedFile "shaders/text/vertex.glsl")
                                         $(embedFile "shaders/text/fragment.glsl")

      farVbo <- GL.genObjectName

      farVao <- U.makeVAO $ do
        GL.bindBuffer GL.ArrayBuffer $= Just farVbo
        U.enableAttrib farProg "pos"
        U.setAttrib farProg "pos" GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float farCharSize fcPosOffset
        do let GL.AttribLocation pos = U.getAttrib farProg "pos"
           GLR.glVertexAttribDivisor pos 1

        GL.bindBuffer GL.ArrayBuffer $= Just farVbo
        U.enableAttrib farProg "texCoord"
        U.setAttrib farProg "texCoord" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float farCharSize fcTexOffset
        do let GL.AttribLocation tex = U.getAttrib farProg "texCoord"
           GLR.glVertexAttribDivisor tex 1

        GL.bindBuffer GL.ArrayBuffer $= Just farVbo
        U.enableAttrib farProg "size"
        U.setAttrib farProg "size" GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float farCharSize fcSizeOffset
        do let GL.AttribLocation size = U.getAttrib farProg "size"
           GLR.glVertexAttribDivisor size 1

        GL.bindBuffer GL.ArrayBuffer $= Just farVbo
        U.enableAttrib farProg "color"
        U.setAttrib farProg "color" GL.ToFloat $ GL.VertexArrayDescriptor 4 GL.Float farCharSize fcColorOffset
        do let GL.AttribLocation color = U.getAttrib farProg "color"
           GLR.glVertexAttribDivisor color 1

        void $ U.makeBuffer GL.ElementArrayBuffer [1, 0, 2, 3 :: GL.GLuint]

      pure FontAtlasRenderer {..}

    makeDumper = do
      fadProg <- U.simpleShaderProgramBS $(embedFile "shaders/dump/vertex.glsl")
                                         $(embedFile "shaders/dump/fragment.glsl")
      fadVbo <- GL.genObjectName

      fadVao <- U.makeVAO $ do
        GL.bindBuffer GL.ArrayBuffer $= Just fadVbo
        U.enableAttrib fadProg "pos"
        U.setAttrib fadProg "pos" GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

        void $ U.makeBuffer GL.ElementArrayBuffer [1, 0, 2, 3 :: GL.GLuint]

      pure FontAtlasDumper {..}

    atlasPageWidth = 512
    atlasPageHeight = 512
    ac = A.AtlasConfig
      { atlasPageWidth
      , atlasPageHeight
      , atlasOverReserve = 0.1
      , atlasMaxOverReserve = 0.3
      }

    makeAtlas = foldM (addSize chars) (A.newAtlas ac, mempty) ss
      where
        chars = [chr 1 .. chr 127] <> ['а' .. 'я'] <> ['А' .. 'Я'] <> ['ё', 'Ё']

        addSize cs am s = setPixelSizes ff s *> foldM addChar am cs
          where
            addChar (a, m) c = do
              (p, cm@CharacterMetrics {..}) <- renderChar ff c
              (pn, xy, a') <- A.insertBitmap (A.Size cmWidth cmHeight) (castPtr p) a
              pure (a', HM.insert (c, s) (pn, xy, cm) m)

-- | Dump the atlas in the center of the OpenGL context.
dumpAtlas :: Int -> Int -> Int -> FontAtlas -> IO ()
dumpAtlas w h l FontAtlas {..} = do
  let FontAtlasDumper {..} = faDumper
  GL.currentProgram $= Just (U.program fadProg)
  U.setUniform fadProg "mvp"  $ L.ortho 0 (realToFrac w :: GL.GLfloat) 0 (realToFrac h) (-1) 1
  U.setUniform fadProg "color" $ V3 (1 :: GL.GLfloat) 1 1
  U.setUniform fadProg "layer" (fromIntegral l :: GL.GLfloat)

  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2DArray $= Just faTex

  GL.bindBuffer GL.ArrayBuffer $= Just fadVbo
  let x = fromIntegral $ (w - 512) `quot` 2 :: GL.GLfloat
      y = fromIntegral $ (h - 512) `quot` 2
  U.replaceBuffer GL.ArrayBuffer
    [ V2 x (y + 512)
    , V2 x y
    , V2 (x + 512) y
    , V2 (x + 512) (y + 512)
    ]

  U.withVAO fadVao $ GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt U.offset0

renderBuf :: FontAtlas -> Int -> Int -> VS.Vector FARChar -> IO ()
renderBuf FontAtlas {..} w h buf = do
  let FontAtlasRenderer {..} = faRenderer
  GL.currentProgram $= Just (U.program farProg)
  U.setUniform farProg "mvp" $ L.ortho 0 (realToFrac w :: GL.GLfloat) (realToFrac h) 0 (-1) 1
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2DArray $= Just faTex

  GL.bindBuffer GL.ArrayBuffer $= Just farVbo
  U.replaceVector GL.ArrayBuffer buf

  U.withVAO farVao $
    GL.drawElementsInstanced GL.TriangleStrip 4 GL.UnsignedInt U.offset0 $ fromIntegral $ VS.length buf

prepare :: FontAtlas -> V2 Int -> TextString -> VS.Vector FARChar
prepare FontAtlas {..} (V2 x y) str = VS.unfoldr go (str, 0)
  where
    go ([], _)                          = Nothing
    go (TextCharacter {..}:str', !offs) = Just (fc, (str', offs + cmAdvance))
      where
        !fc = FARChar {..}
        Just (p, (tx, ty), CharacterMetrics {..}) = HM.lookup (tcChar, tcSize) faLoc
        fcPos = fromIntegral <$> V2 (x + offs + cmBearingX) (y - cmBearingY)
        fcSize = fromIntegral <$> V2 cmWidth cmHeight
        fcColor = tcColor
        fcTexCoord = V3 (fromIntegral tx) (fromIntegral ty) (fromIntegral p)

-- | Render specified text string using font alias immediately.
render :: FontAtlas -> V2 Int -> Int -> Int -> TextString -> IO ()
render fa pos w h str = renderBuf fa w h $ prepare fa pos str

-- | Renderer which enqueues text pieces and can later render all of those using only one
-- OpenGL rendering function invocation.
data DelayedRenderer = DelayedRenderer
  { drAtlas :: !FontAtlas
  , drBuf   :: !(IORef (VS.Vector FARChar))
  }

-- | Construct new delayed renderer.
newDelayedRenderer :: FontAtlas -> IO DelayedRenderer
newDelayedRenderer fa = DelayedRenderer fa <$> newIORef mempty

-- | Enqueue a text string to be rendered later.
enqueue :: DelayedRenderer -> V2 Int -> TextString -> IO ()
enqueue DelayedRenderer {..} pos str = modifyIORef' drBuf (<> prepare drAtlas pos str)

-- | Render all previously enqueued text strings and empty the store, ready to accept new strings.
flushRender :: DelayedRenderer -> Int -> Int -> IO ()
flushRender DelayedRenderer {..} w h = (readIORef drBuf <* writeIORef drBuf mempty)
  >>= renderBuf drAtlas w h
