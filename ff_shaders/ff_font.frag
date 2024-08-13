precision highp float;
in vec2 text_pos;
in vec4 text_color;
in float strength;
out vec4 color;

uniform sampler2D font_atlas;
uniform mat4 font_projection;

float median(float r, float g, float b) {
    return max(min(r, g), min(max(r, g), b));
}
float pxRange = 4.0;

float srgb_from_linear(float value) {
  return value<= 0.0031308f
       ? value* 12.92f
       : pow (value, 1.0f/2.4f) * 1.055f - 0.055f;
}
float Luma(vec3 color) { return dot(color, vec3(0.2126, 0.7152, 0.0722)); }
void main() {
    vec2 coords = (font_projection * vec4(text_pos, 0.0, 1.0)).xy;

    /* Invert the strength so that 1.0 becomes bold and 0.0 becomes thin */
    float threshold = 1.0 - strength;

    vec2 msdfUnit = pxRange/vec2(textureSize(font_atlas, 0));
    vec3 s = texture(font_atlas, coords).rgb;
    float sigDist = median(s.r, s.g, s.b) - threshold;
    sigDist *= dot(msdfUnit, 0.5/fwidth(coords));
    float opacity = clamp(sigDist + 0.5, 0.0, 1.0);

    vec3 v = text_color.rgb;
    v = mix(vec4(0.0, 0.0, 0.0, 0.0), text_color, opacity).rgb;
    v.r = srgb_from_linear(v.r); 
    v.g = srgb_from_linear(v.g); 
    v.b = srgb_from_linear(v.b); 

    float uSaturation = 4.0f;
    float luma = Luma(v);
    v = mix(vec3(luma), v, uSaturation);

    color = vec4(v, opacity * text_color.a);
}
