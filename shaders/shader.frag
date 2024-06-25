#version 450
#extension GL_EXT_debug_printf: enable
#extension GL_EXT_nonuniform_qualifier: enable

struct Light {
    vec3 position;
    vec3 color;
    float intensity;
};

layout(binding = 1) uniform LightBlock {
    // int numLights;
    Light lights[10];
};

layout(binding = 0) uniform sampler2D texSampler[];

layout(location = 0) in vec3 fragColor; // not used
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) in flat uint fragTexIndex;



layout(location = 0) out vec4 outColor;

void main() {
    vec4 texColor = texture(texSampler[fragTexIndex], fragTexCoord);
    vec4 finalColor = texColor;

    for (int i = 0; i < 1; ++i) {
        // Calculate the effect of light[i] on the fragment
        // This is a simplified example; actual lighting calculations would involve
        // the light's position, the fragment's position, normal vectors, etc.
        vec3 lightEffect = lights[i].color * lights[i].intensity;
        finalColor.rgb += lightEffect;
    }
    outColor = texColor;
    // outColor = texColor;
    // debugPrintfEXT("(r=%f g=%f b=%f a=%f)", outColor.r, outColor.g, outColor.b, outColor.a);
    // debugPrintfEXT("A", outColor.r, outColor.g, outColor.b, outColor.a);
}