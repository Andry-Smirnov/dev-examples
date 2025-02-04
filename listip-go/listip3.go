package main

import (
	"fmt"
	"log"
	"net/netip"
)

func main() {

	cidrAddress := "8.8.8.8/24"
	p, err := netip.ParsePrefix(cidrAddress)
	if err != nil {
		log.Fatalf("invalid cidr: %s, error %v", cidrAddress, err)
	}
	// 8.8.8.8/24 => 8.8.8.0/24
	p = p.Masked()

	addr := p.Addr()
	for {
		if !p.Contains(addr) {
			break
		}
		fmt.Println(addr.String())
		addr = addr.Next()
	}
}