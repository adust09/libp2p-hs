FROM alpine:3.19

RUN apk add --no-cache \
    iproute2 \
    iptables

COPY interop/hole-punch/router/run.sh /usr/local/bin/run-hole-punch-router
RUN chmod +x /usr/local/bin/run-hole-punch-router

ENTRYPOINT ["/usr/local/bin/run-hole-punch-router"]
