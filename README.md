# vulkan

Trying to learn vulkan and Haskell hand in hand by doing a 2D engine. Hopefully at some point I can add 2D global illumination as well.

## Building and running
### Compile the shaders
```
./compile 
```
### Run
```
stack run
```
## TODO (doubling as a roadmap)
- [ ] Allocate both index and vertex buffers in one buffer
- [ ] Maybe images from the same buffer as well
- [x] More than one in-flight frame
- [x] Some memory is not properly freed before vulkan instance is destroyed
- [x] Just use VMA ~~Don't map/unmap memory for vertex/index buffers every frame. Instead map/unmap once and just copy the data every frame~~
- [x] Multiple sprites using the different textures in one frame
- [ ] Use a texture atlas probably? Not sure yet
- [ ] Investigate sampler2DArray
- [ ] Sprite back-to-front sorting, do I need this for alpha blending?
- [ ] Sprite bacthing
- [ ] Bindless textures
- [ ] VK_KHR_buffer_device_address
- [ ] GPU driven rendering (DrawIndirect, etc)
- [ ] Use Debug Print as Vk 1.3, not the extension
- [ ] Basic DearImGUI integration
- [ ] Add texture support
- [ ] Basic static (no animation) stripe
- [ ] Somewhat model a single moving light source
- [ ] Use normal maps on the static stripe to intract with the light sources
- [ ] Use specular maps for more realism
- [ ] Think about bloom and glow effects
- [ ] Suuport multiple light sources
- [ ] 2D global illumination