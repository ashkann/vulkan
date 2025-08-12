#version 450

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec2 inTexCoord;
layout(location = 2) in uint inTexIndex;
layout(location = 3) in vec4 inTexColor;

layout(location = 0) out vec2 fragTexCoord;
layout(location = 1) out uint fragTexIndex;
layout(location = 2) out vec4 fragTexColor;

void main() {
    gl_Position = vec4(inPosition, 0.0, 1.0);
    fragTexCoord = inTexCoord;
    fragTexIndex = inTexIndex;
    fragTexColor = inTexColor;
}