# 0.10.4

- Fix profiled builds with GHC >= 8.8 (@maoe)
- Fix floating point RGBA texture support

# 0.10.1
Bump `OpenGLRaw` upper bound

# 0.10.0
Use Cabal 2.0's `build-tool-depends` field

# 0.9.2

* Don't use `hpp` for preprocessing on Windows

# 0.9.1

* Bump hpp dependency to address a change in the way GHC 8 invokes the pre-processor.

# 0.9.0

* OpenGL 3 dependency bump

# 0.8.8

* Update to `hpp-0.3.0`

# 0.8.7

* Use `hpp` as a preprocessor rather than `cpphs`

# 0.8.2

* Do not print shader info log if it contains only \NUL

# 0.8.1

* Added `orthoMatrix` to `Camera3D`

* Added `loadShaderFamily` to load all vertex, geometry, and fragment
  shaders based on a root file name.

# 0.8

* Generalized `setUniform` to work with any instance of `AsUniform`. Specifically, this enables the use of types from the `linear` package.
