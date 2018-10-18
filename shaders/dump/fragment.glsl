#version 330 core

in vec3 fragTexCoord;

out vec4 outColor;

uniform vec3 color;
uniform sampler2DArray tex;

void main()
{
  outColor = vec4(color.rgb, texture(tex, fragTexCoord).r);
}
