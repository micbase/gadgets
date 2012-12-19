#!/bin/bash

CONFIG='/etc/openvpn/UDP.conf'
RESOLV='/etc/resolv.conf'

case "$1" in
	4)
		echo "Switching OpenVPN to go through IPv4"
		sudo ~/killvpn.sh
		sudo sed -i '4s/#proto/proto/; 5s/#remote/remote/; 7s/proto/#proto/; 8s/remote/#remote/' $CONFIG
		sudo sed -i '7s/#nameserver/nameserver/; 8s/nameserver/#nameserver/' $RESOLV
		sudo /etc/rc.d/openvpn start
		;;

	6)
		echo "Switching OpenVPN to go through IPv6"
		sudo ~/killvpn.sh
		sudo sed -i '4s/proto/#proto/; 5s/remote/#remote/; 7s/#proto/proto/; 8s/#remote/remote/' $CONFIG
		sudo sed -i '7s/nameserver/#nameserver/; 8s/#nameserver/nameserver/' $RESOLV
		sudo /etc/rc.d/openvpn start
		;;
	*)
		echo "usage: $0 {4|6}"
esac
