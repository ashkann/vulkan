#version 450
#extension GL_EXT_debug_printf: enable
#extension GL_EXT_nonuniform_qualifier: enable

layout(binding = 0) uniform sampler2D texSampler[];

layout(location = 0) in vec3 fragColor; // not used
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) in flat uint fragTexIndex;

layout(location = 0) out vec4 outColor;

void main() {
    vec4 texColor = texture(texSampler[fragTexIndex], fragTexCoord);
    // outColor = vec4(mix(backgroundColor, texColor.rgb, texColor.a), 0.5);
    // outColor = vec4(mix(texColor.rgb, vec3(1.0), 0.0), 1.0);
    // outColor = vec4(fragTexCoord, 0.0, 1.0);
    outColor = texColor;
    // outColor = texColor;
    // debugPrintfEXT("(r=%f g=%f b=%f a=%f)", outColor.r, outColor.g, outColor.b, outColor.a);
    // debugPrintfEXT("A", outColor.r, outColor.g, outColor.b, outColor.a);
}