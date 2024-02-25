#version 450
#extension GL_EXT_debug_printf: enable

layout(binding = 0) uniform sampler2D texSampler;

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

void main() {
    vec4 texColor = texture(texSampler, fragTexCoord);
    // debugPrintfEXT("(r=%f g=%f b=%f a=%f)", outColor.r, outColor.g, outColor.b, outColor.a);
}