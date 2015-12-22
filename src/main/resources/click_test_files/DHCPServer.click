FromDevice() -> class :: DHCPClassifier( discover, release, -)
udp_encap :: UDPIPEncap( 192.168.10.10, 67, 255.255.255.255, 68 )
eth_encap :: EtherEncap( 800, 52.54.00.E5.33.17 , ff.ff.ff.ff.ff.ff)

offer :: DHCPSererOffer( 10.0.0.1 )

class[1] -> DHCPServerRelease()

class[0] -> offer[0] -> CheckDHCPMsg -> udp_encap -> eth_encap -> ToDevice()

