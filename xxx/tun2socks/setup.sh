#!/usr/bin/env bash

# RUN AS ROOT!!!

ip tuntap add mode tun dev tun0
ip addr add 198.18.0.1/15 dev tun0
ip link set dev tun0 up

ip route del default
ip route add default via 198.18.0.1 dev tun0 metric 1
ip route add default via 192.168.1.1 dev enp2s0 metric 10

# start the tun2socks system service here...
