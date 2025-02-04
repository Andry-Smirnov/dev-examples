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