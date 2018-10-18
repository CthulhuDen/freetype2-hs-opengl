#version 330 core

in vec2 pos;

out vec3 fragTexCoord;

uniform mat4 mvp;
uniform float layer;

void main()
{
  gl_Position = mvp * vec4(pos, 0.0, 1.0);
  fragTexCoord = vec3(vec2[4](
    vec2(0, 0),
    vec2(0, 1),
    vec2(1, 1),
    vec2(1, 0)
  )[gl_VertexID], layer);
}
