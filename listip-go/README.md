# [go - How to list all IPs in a network - Stack Overflow](https://stackoverflow.com/questions/60540465/how-to-list-all-ips-in-a-network)

Python has a method in the `ipaddress` module to list all the IPs in a network. eg.

```go
import ipaddress
ips = [ip for ip in ipaddress.ip_network('8.8.8.0/24').hosts()]
```

How would you do the same thing in Go?

## Answer 1

convert the CIDR address and netmask to uint32. Find the start and finish and then loop on the uint32 to get the addresses

```go
package main

import (
    "encoding/binary"
    "fmt"
    "log"
    "net"
)

func main() {
    // convert string to IPNet struct
    _, ipv4Net, err := net.ParseCIDR("192.168.255.128/25")
    if err != nil {
        log.Fatal(err)
    }

    // convert IPNet struct mask and address to uint32
    // network is BigEndian
    mask := binary.BigEndian.Uint32(ipv4Net.Mask)
    start := binary.BigEndian.Uint32(ipv4Net.IP)

    // find the final address
    finish := (start & mask) | (mask ^ 0xffffffff)

    // loop through addresses as uint32
    for i := start; i <= finish; i++ {
            // convert back to net.IP
        ip := make(net.IP, 4)
        binary.BigEndian.PutUint32(ip, i)
        fmt.Println(ip)
    }

}
```

[https://play.golang.org/p/5Yq0kXNnjYx](https://play.golang.org/p/5Yq0kXNnjYx)

## Answer 2

I found a way to do it based on [https://play.golang.org/p/fe-F2k6prlA](https://play.golang.org/p/fe-F2k6prlA) by adam-hanna in this thread - [https://gist.github.com/kotakanbe/d3059af990252ba89a82](https://gist.github.com/kotakanbe/d3059af990252ba89a82)

```go
package main

import (
    "fmt"
    "log"
    "net"
    "time"
)

func Hosts(cidr string) ([]string, int, error) {
    ip, ipnet, err := net.ParseCIDR(cidr)
    if err != nil {
        return nil, 0, err
    }

    var ips []string
    for ip := ip.Mask(ipnet.Mask); ipnet.Contains(ip); inc(ip) {
        ips = append(ips, ip.String())
    }

    // remove network address and broadcast address
    lenIPs := len(ips)
    switch {
    case lenIPs < 2:
        return ips, lenIPs, nil

    default:
    return ips[1 : len(ips)-1], lenIPs - 2, nil
    }
}

func inc(ip net.IP) {
    for j := len(ip) - 1; j >= 0; j-- {
        ip[j]++
        if ip[j] > 0 {
            break
        }
    }
}

func main() {
    ips, count, err := Hosts("8.8.8.0/24")
    if err != nil {
        log.Fatal(err)
    }

    for n := 0; n <= count; n += 8 {
        fmt.Println(ips[n])
    }
}
```

## Answer 3

as of go 1.16 use the new standard library [net/netip](https://pkg.go.dev/net/netip) and it's very simple  
(and it works for IPv6 too thanks to the simplicity of the new net/netip library)

```go
func main() {
    var err error
    cidrAddress := "8.8.8.8/24"
    p, err := netip.ParsePrefix(cidrAddress)
    if err != nil {
        err = fmt.Errorf("invalid cidr: %s, error %v", cidrAddress, err)
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
```

```console
8.8.8.0
8.8.8.1
8.8.8.2
...
8.8.8.254
8.8.8.255
```

[IPv4-example](https://go.dev/play/p/fYM8lJTa2wU)  
[IPv6-example](https://go.dev/play/p/5PkLIk0hde-)

## Answer 4

Here is the equivalent using the [IPAddress Go module](https://seancfoley.github.io/IPAddress/). Disclaimer: I am the project manager.

```go
import a "github.com/seancfoley/ipaddress-go/ipaddr"

var addrs []*a.IPAddress
block := a.NewIPAddressString("8.8.8.0/24").GetAddress()
for i := block.Iterator(); i.HasNext(); {
    addrs = append(addrs, i.Next())
}
```

Use `append(addrs, iter.Next().GetNetIP())` for a slice of `net.IP`.

## Answer 5

You can use [net.ParseCIDR](https://golang.org/pkg/net/#ParseCIDR) to generate an *IPNet for the corresponding network.
