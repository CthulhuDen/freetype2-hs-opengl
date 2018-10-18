#version 330 core

in vec2 pos;
in vec3 texCoord;
in vec2 size;
in vec4 color;

out vec3 fragTexCoord;
out vec4 fragColor;

uniform mat4 mvp;

void main()
{
  vec2 offset = vec2[4](
    vec2(0,      0),
    vec2(0,      size.y),
    vec2(size.x, size.y),
    vec2(size.x, 0)
  )[gl_VertexID];

  gl_Position = mvp * vec4(pos.xy + offset.xy, 0.0, 1.0);
  fragTexCoord = vec3((texCoord.st + offset.st) / 512.0, texCoord.p);
  fragColor = color;
}
