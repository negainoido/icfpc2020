FROM gcr.io/distroless/cc

# TODO: switch to release here.
COPY target/debug/solver1 /
COPY target/debug/cympfh /
