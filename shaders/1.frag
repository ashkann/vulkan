precision mediump float;

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

float circleshape(vec2 position, float radius){
    return step(radius, length(position - vec2(0.5)));
}

void main()
{
    vec2 position = gl_FragCoord.xy / u_resolution.xy;
    vec2 translate = vec2(sin(u_time / 10.0), 0.0);
    position -= translate;
    float circle = circleshape(position, 0.3);
    vec3 color = vec3(circle);
    gl_FragColor = vec4(color, 1.0);
}