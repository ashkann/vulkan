#version 450
#extension GL_EXT_debug_printf:enable
#extension GL_EXT_nonuniform_qualifier:enable

struct Light{
    float intensity;
    vec2 position;
    vec3 color;
};

layout(binding=1)uniform LightBlock{
    // int numLights;
    Light lights[1];
};

layout(binding=2)uniform ScreenBlock{
    uvec2 viewportSizeInPixels;
};

layout(binding=0)uniform sampler2D texSampler[];

layout(location=0)in vec3 fragColor;// not used
layout(location=1)in vec2 fragTexCoord;
layout(location=2)in flat uint fragTexIndex;

layout(location=0)out vec4 outColor;

float insideBox(vec2 v,vec2 bottomLeft,vec2 topRight){
    vec2 s=step(bottomLeft,v)-step(topRight,v);
    return s.x*s.y;
}

void main(){
    vec2 origin=vec2(.5,.5);
    vec2 normalizedFragCoord=(gl_FragCoord.xy/viewportSizeInPixels)-origin;
    
    vec4 texColor=texture(texSampler[fragTexIndex],fragTexCoord);
    vec4 finalColor=texColor;
    
    // for (int i = 0; i < 1; ++i) {
        // Calculate the effect of light[i] on the fragment
        // This is a simplified example; actual lighting calculations would involve
        // the light's position, the fragment's position, normal vectors, etc.
        float intensity=lights[0].intensity;
        
        float d=distance(lights[0].position,normalizedFragCoord)*5.;
        float falloffConstant=1.;
        float falloff=intensity/(falloffConstant+(d*d));
        vec3 lightEffect=lights[0].color*falloff;
        finalColor.rgb*=lightEffect;
    // }
    outColor=finalColor;
    // outColor = texColor;
    // debugPrintfEXT("(r=%f g=%f b=%f a=%f)", outColor.r, outColor.g, outColor.b, outColor.a);
    if(insideBox(gl_FragCoord.xy,vec2(240,240),vec2(260,260))>0){
        // debugPrintfEXT("%1.1f %1.1f",lights[0].intensity, lights[0].intensity/d);
        // debugPrintfEXT("%v2i",viewportSizeInPixels);
        // debugPrintfEXT("%1.1v2f|%1.1v3f|%1.1f",lights[0].position,lights[0].color,lights[0].intensity);
        // debugPrintfEXT("%1.1f",lights[0].intensity);
    }
}