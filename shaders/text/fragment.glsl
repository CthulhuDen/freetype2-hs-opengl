#version 330 core

in vec3 fragTexCoord;
in vec4 fragColor;

out vec4 outColor;

uniform sampler2DArray tex;

void main()
{
  outColor = vec4(fragColor.rgb, texture(tex, fragTexCoord).r);
}
