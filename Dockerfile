FROM haskell:9

WORKDIR /opt/demo

RUN cabal update

COPY app ./app/
COPY src ./src/
COPY tests ./tests/
COPY asm ./asm/
COPY 3364mach.cabal 3364mach.cabal

RUN cabal install exe:3364mach

ENTRYPOINT ["3364mach"]