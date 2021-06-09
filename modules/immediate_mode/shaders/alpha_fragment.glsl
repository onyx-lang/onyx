#version 300 es

precision mediump float;

uniform sampler2D u_texture;

in vec4 v_color;
in vec2 v_texture;

out vec4 fragColor;

void main() {
    float alpha = texture(u_texture, v_texture).a;
    fragColor = v_color * alpha;
}