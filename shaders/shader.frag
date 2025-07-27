#version 450
#extension GL_EXT_debug_printf:enable
#extension GL_EXT_nonuniform_qualifier:enable

layout(binding = 0) uniform sampler2D texSampler[];

layout(location = 0) in vec2 fragTexCoord;
layout(location = 1) in flat uint fragTexIndex;

layout(location = 0) out vec4 outColor;

void main() {
    outColor = texture(texSampler[fragTexIndex], fragTexCoord);
}