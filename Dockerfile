# vim: nospell

FROM tmcdonell/accelerate-llvm
MAINTAINER Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>

RUN apt-get update \
 && apt-get install -y \
      freeglut3-dev \
      libfftw3-dev \
      libgmp-dev

RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/libcuda.so.1

# Copy over just the cabal and stack file and install dependencies
WORKDIR /opt/accelerate-examples
COPY ./stack-8.10.yaml /opt/accelerate-examples/stack.yaml
COPY ./accelerate-examples.cabal /opt/accelerate-examples/
RUN stack build accelerate-examples \
  --only-dependencies \
  --flag accelerate-examples:-gui

# Copy over the source files and build
COPY . /opt/accelerate-examples
RUN stack install --flag accelerate-examples:-gui

# https://github.com/tmcdonell/cuda/issues/55
RUN rm /usr/local/cuda/lib64/libcuda.so.1

CMD ["bash"]

