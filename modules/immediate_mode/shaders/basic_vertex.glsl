#version 300 es

layout(location = 0) in vec2 a_vertex;
layout(location = 1) in vec4 a_color;
layout(location = 2) in vec2 a_texture;

uniform mat4 u_view;
uniform mat4 u_world;

out vec4 v_color;
out vec2 v_texture;

void main() {
    gl_Position = u_view * u_world * vec4(a_vertex, 0, 1);

    v_color = a_color;
    v_texture = a_texture;
}