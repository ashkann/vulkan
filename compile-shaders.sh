#!/usr/bin/env sh
glslang -V shaders/shader.vert -o out/vert.spv
glslang -V shaders/shader.frag -o out/frag.spv