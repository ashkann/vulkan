# vulkan

Trying to learn vulkan and Haskell hand in hand by doing a 2D engine. Hopefully at some point I can add a 2d global illumination renderer as well.

## Building and running
```
stack build
```
```
stack run
```

## TODO (doubling as a readmap)
- [ ] Allocate both index and vertex buffers in one buffer
- [ ] More than one in-flight frame
- [ ] Some memory is not properly freed before vulkan instance is destroyed
- [ ] Don't map/unmap memory for vertex/index buffers every frame. Instead map/unmap once and just copy the data every frame
- [ ] Basic DearImGUI integration
- [ ] Add texture support
- [ ] Basic static (no animation) stripe
- [ ] Somewhat model a single moving light source
- [ ] Use normal maps on the static stripe to intract with the light sources
- [ ] Suuport multiple light sources
- [ ] 2D global illumination