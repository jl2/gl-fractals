#version 400 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 uv;

uniform mat4 transform;

out vec2 complexCoordinate;

void main(void)
{
     gl_Position = transform * vec4(position, 1.0);
     complexCoordinate = uv;
}
