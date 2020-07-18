FROM haskell:8.8.3-stretch as build

WORKDIR /work

COPY interpreter.cabal /work

RUN cabal v2-update && \
  cabal v2-configure --enable-executable-static 
RUN cabal v2-build -v1 --only-dependencies all

COPY . /work

RUN cabal v2-install -j4

FROM debian:stretch-slim

COPY --from=build /root/.cabal/store/ghc-8.8.3/ /root/.cabal/store/ghc-8.8.3/
COPY --from=build /root/.cabal/bin /root/.cabal/bin
COPY --from=build /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.10

ENTRYPOINT ["/root/.cabal/bin/interpreter"]