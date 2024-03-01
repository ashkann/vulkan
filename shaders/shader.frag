#version 450
#extension GL_EXT_debug_printf: enable

layout(binding = 2) uniform sampler2D texSampler[2];

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) in flat uint fragTexIndex;

layout(location = 0) out vec4 outColor;

void main() {
    int i = 1;
    vec4 texColor = texture(texSampler[fragTexIndex], fragTexCoord);
    outColor = vec4(mix(fragColor, texColor.rgb, texColor.a), 1.0);
    // outColor = texColor;
    // debugPrintfEXT("(r=%f g=%f b=%f a=%f)", outColor.r, outColor.g, outColor.b, outColor.a);
}