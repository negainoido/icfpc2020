FROM gcr.io/distroless/cc

COPY target/* /

ENV SOLVER_REVISION $SOLVER_REVISION
