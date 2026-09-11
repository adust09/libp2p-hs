#!/bin/sh
set -eu

add_route() {
  subnet="$1"
  gateway="$2"
  interface="$3"
  label="$4"

  if [ -z "$subnet" ] || [ -z "$gateway" ]; then
    return
  fi

  echo "Setting route to ${label} ${subnet} via ${gateway}" >&2
  ip route replace "$subnet" via "$gateway" dev "$interface"
}

if [ -n "${IS_DIALER+x}" ]; then
  add_route "${WAN_SUBNET:-}" "${WAN_ROUTER_IP:-}" lan0 "WAN subnet"
else
  add_route \
    "${DIALER_LAN_SUBNET:-}" \
    "${DIALER_ROUTER_IP:-}" \
    wan0 \
    "dialer LAN"
  add_route \
    "${LISTENER_LAN_SUBNET:-}" \
    "${LISTENER_ROUTER_IP:-}" \
    wan0 \
    "listener LAN"
fi

exec /usr/local/bin/libp2p-hole-punch "$@"
