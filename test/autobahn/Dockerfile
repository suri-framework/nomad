FROM alpine

RUN apk add opam git musl-dev make m4 gcc bubblewrap bash coreutils pkgconfig openssl-libs-static openssl-dev gmp-dev
RUN opam init --disable-sandboxing
RUN opam install -y dune

WORKDIR /app

COPY *.opam ./

RUN opam update
RUN opam pin atacama.0.0.5 git+https://github.com/suri-framework/atacama -y || true
RUN opam pin trail.0.0.1 git+https://github.com/suri-framework/trail -y || true

RUN opam install . --deps-only -y

COPY . .

RUN opam exec -- dune build test/autobahn/server.exe

FROM alpine

RUN apk add gmp

WORKDIR /app

COPY --from=0 /app/_build/default/test/autobahn/server.exe .

CMD ["/app/server.exe"]
