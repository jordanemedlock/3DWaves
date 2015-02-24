3DWaves
=======

This is a personal project, still in its infancy, and I don't expect anybody else to use it. Should somehow happen upon this site, I would however welcome their support and feedback.

##Contents
Wavefront OBJ parsers and related amenities. Includes purely functional parsers
and IO utilities for loading models from files.

Supports the basic MTL and OBJ attributes. My ambition is to add full support for the entire specification.

Please note that this package is completely unaware of rendering and graphics. The data structures generated by the parsers are oblivious to technologies such as Direct3D and OpenGL; creating eg. GPU buffers is up to the client.

I may at some point implement the FFI and add direct OpenGL support, in separate modules.

##Maintainers
Jonatan H Sundqvist

##TODO
There is currently no standalone list of tasks (cf. source files).