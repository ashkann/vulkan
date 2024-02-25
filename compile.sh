#!/usr/bin/env sh
glslc shaders/shader.vert -o vert.spv
glslc shaders/shader.frag -o frag.spv
# glslc shaders/tex_image.frag -o frag.spv