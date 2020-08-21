<div align="center">
<img width="450" src="https://github.com/AccelerateHS/accelerate/raw/master/images/accelerate-logo-text-v.png?raw=true" alt="henlo, my name is Theia"/>

# Example programs using Accelerate

[![GitHub CI](https://github.com/tmcdonell/accelerate-examples/workflows/CI/badge.svg)](https://github.com/tmcdonell/accelerate-examples/actions)
[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/AccelerateHS/Lobby)
<br>
[![Stackage LTS](https://stackage.org/package/accelerate-examples/badge/lts)](https://stackage.org/lts/package/accelerate-examples)
[![Stackage Nightly](https://stackage.org/package/accelerate-examples/badge/nightly)](https://stackage.org/nightly/package/accelerate-examples)
[![Hackage](https://img.shields.io/hackage/v/accelerate-examples.svg)](https://hackage.haskell.org/package/accelerate-examples)
<br>
[![Docker Automated build](https://img.shields.io/docker/automated/tmcdonell/accelerate-examples.svg)](https://hub.docker.com/r/tmcdonell/accelerate-examples/)
[![Docker status](https://images.microbadger.com/badges/image/tmcdonell/accelerate-examples.svg)](https://microbadger.com/images/tmcdonell/accelerate-examples)

</div>

Example programs using the Accelerate library. The aim is for this package to
evolve and be useful for both performance and regression testing.

Contributions and bug reports are welcome!<br>
Please feel free to contact me through [GitHub](https://github.com/AccelerateHS/accelerate/issues) or [gitter.im](https://gitter.im/AccelerateHS/Lobby).


Installation
------------

### External dependencies

Installation of `accelerate-examples` and its dependencies requires several
external packages. You may need to adjust the package names or versions slightly
for your system.

  * Ubuntu/Debian (apt-get):
    - llvm-9-dev
    - freeglut3-dev
    - libfftw3-dev

  * Mac OS ([homebrew](http://brew.sh/index.html))
    - fftw
    - libffi
    - llvm-hs/homebrew-llvm/llvm-9

If you want to use the CUDA GPU enabled backend
[`accelerate-llvm-ptx`](https://github.com/AccelerateHS/accelerate-llvm), you
will also need to install the CUDA toolkit for your system. You can find an
installer on NVIDIA's website here:

  * https://developer.nvidia.com/cuda-downloads


### Building: stack

For development, the recommend build method is via the
[`stack`](http://haskellstack.org) tool. This will simplify pulling in
dependencies not yet on Hackage. For example, to build using ghc-8.10:

```bash
ln -s stack-8.10.yaml stack.yaml    # only once
stack build                         # or, 'stack install' to install the executables globally
```

Before building, you may want to edit the `stack.yaml` file to change the build
configuration. In particular, the `flags` section at the bottom can be used to
enable or disable individual example programs and accelerate backends, as well
as features such as monitoring and debug output.


Adding new backends
-------------------

Adding support for new Accelerate backends should require only a few minor
additions to the cabal file and the module
'Data.Array.Accelerate.Examples.Internal.Backend'.

