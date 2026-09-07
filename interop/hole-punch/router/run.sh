#!/bin/sh
set -eu

require_env() {
  name="$1"
  eval "value=\${$name:-}"
  if [ -z "$value" ]; then
    echo "Missing required environment variable: $name" >&2
    exit 1
  fi
}

require_env WAN_IP
require_env WAN_SUBNET
require_env LAN_IP
require_env LAN_SUBNET

iptables -t nat -F
iptables -t filter -F
iptables -P FORWARD DROP
iptables -P INPUT ACCEPT
iptables -P OUTPUT ACCEPT

# Do not reject the first SYN before the peer creates its matching mapping.
iptables -A INPUT -i wan0 -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
iptables -A INPUT -i wan0 -j DROP
iptables -t nat -A POSTROUTING -s "$LAN_SUBNET" -o wan0 -j MASQUERADE
iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
iptables -A FORWARD -s "$LAN_SUBNET" -i lan0 -o wan0 -j ACCEPT
iptables -A FORWARD -d "$LAN_SUBNET" -i wan0 -o lan0 -j ACCEPT

echo "NAT router ready: ${LAN_SUBNET} via ${WAN_IP}" >&2
exec tail -f /dev/null
