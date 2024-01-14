# vulkan

Trying to learn vulkan and Haskell hand in hand by doing a 2D engine. Hopefully at some point I can add a 2d global illumination renderer as well.

## Building and running
```
stack build
```
```
stack run
```

## TODO
- [ ] Allocate both index and vertex buffers in one buffer
- [ ] More than one in-flight frame
- [ ] Some memory is not properly freed before vulkan instance is destroyed