client :: DHCPClient()
udp_encap :: UDPIPEncap(255.255.255.255, 68, 255.255.255.255, 67)
eth_encap :: EtherEncap(800, 1.2.3.4.5.6, ff.ff.ff.ff.ff.ff)

FromDevice() -> client[0] -> CheckDHCPMsg() ->  udp_encap -> eth_encap -> ToDevice()
