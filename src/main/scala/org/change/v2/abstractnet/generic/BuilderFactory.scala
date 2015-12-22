package org.change.v2.abstractnet.generic

import org.change.v2.Template
import org.change.v2.abstractnet.click.sefl._

object BuilderFactory {

  def getBuilder(nameValue: String, elementType: String) = elementType match {
    case "ToDevice" | "ToNetPort" | "ToNetFront" => ToDevice.getBuilder(nameValue)
    case "FromDevice" | "FromNetPort" | "FromNetFront" => FromDevice.getBuilder(nameValue)
    //    case "SEQChanger" => SEQChanger.getBuilder(nameValue)
    //    case "StartTunnel" => StartTunnel.getBuilder(nameValue)
    //    case "EndTunnel" => EndTunnel.getBuilder(nameValue)
    //    case "Firewall" => Firewall.getBuilder(nameValue)
    //    case "NAT" => NAT.getBuilder(nameValue)
    //    case "Client" => Client.getBuilder(nameValue)
    //    case "Server"  => Server.getBuilder(nameValue)
    case "Tee" => Tee.getBuilder(nameValue)
    case "Discard" => Discard.getBuilder(nameValue)
    case "ClampMTU" => ClampMTU.getBuilder(nameValue)
    case "DHCPSetState" => DHCPSetState.getBuilder(nameValue)
    case "DHCPCheckState" => DHCPCheckState.getBuilder(nameValue)
    case "IPRewriter" => IPRewriter.getBuilder(nameValue)
    case "IPEncap" => IPEncap.getBuilder(nameValue)
    case "IPDecap" => IPDecap.getBuilder(nameValue)
    case "ICMPPingSource" => ICMPPingSource.getBuilder(nameValue)
    case "ICMPPingResponder" => ICMPPingResponder.getBuilder(nameValue)
    case "IPsecESPEncap" => IPsecESPEncap.getBuilder(nameValue)
    case "IPsecESPDecap" => IPsecESPDecap.getBuilder(nameValue)
    case "IPsecAES" => IPsecAES.getBuilder(nameValue)
    case "IPClassifier" => IPClassifier.getBuilder(nameValue)
    case "StripIPHeader" => StripIPHeader.getBuilder(nameValue)
    case "CheckIPHeader" => CheckIPHeader.getBuilder(nameValue)
    case "IPMirror" => IPMirror.getBuilder(nameValue)
    case "DecIPTTL" => DecIPTTL.getBuilder(nameValue)
    case "EtherEncap" => EtherEncap.getBuilder(nameValue)
    case "EtherDecap" => EtherDecap.getBuilder(nameValue)
    case "VLANEncap" => VLANEncap.getBuilder(nameValue)
    case "VLANDecap" => VLANDecap.getBuilder(nameValue)
    case "HostEtherFilter" => HostEtherFilter.getBuilder(nameValue)
    case "Template"  => Template.getBuilder(nameValue)
    case "AddTCPOptions"  => AddTCPOptions.getBuilder(nameValue)
    case "ScanTCPOptions"  => ScanTCPOptions.getBuilder(nameValue)
    //    case "IPFilter"  => IPFilter.getBuilder(nameValue)
    case "Paint" => Paint.getBuilder(nameValue)
    case "CheckDHCPMsg"  => CheckDHCPMsg.getBuilder(nameValue)
    case "DHCPClassifier"  => DHCPClassifier.getBuilder(nameValue)
    case "DHCPClient"  => DHCPClient.getBuilder(nameValue)
    case "DHCPICMPEncap"  => DHCPICMPEncap.getBuilder(nameValue)
    case "DHCPLeaseHash"  => DHCPLeaseHash.getBuilder(nameValue)
    case "DHCPLeasePool"  => DHCPLeasePool.getBuilder(nameValue)
    case "DHCPServerACKorNAK"  => DHCPServerACKorNAK.getBuilder(nameValue)
    case "DHCPServerOffer"  => DHCPServerOffer.getBuilder(nameValue)
    case "DHCPServerRelease"  => DHCPServerRelease.getBuilder(nameValue)
    case "UDPIPEncap"  => UDPIPEncap.getBuilder(nameValue)
    case _ => NoOpClickElm.getBuilder(nameValue, elementType)
  }
  def getBuilder(elementType: String) = elementType match {
    case "ToDevice" | "ToNetPort" | "ToNetFront" => ToDevice.getBuilder
    case "FromDevice" | "FromNetPort" | "FromNetFront" => FromDevice.getBuilder
//    case "SEQChanger" => SEQChanger.getBuilder
//    case "StartTunnel" => StartTunnel.getBuilder
//    case "EndTunnel" => EndTunnel.getBuilder
//    case "Firewall" => Firewall.getBuilder
//    case "NAT" => NAT.getBuilder
//    case "Client" => Client.getBuilder
//    case "Server" => Server.getBuilder
    case "Tee" => Tee.getBuilder
    case "Discard" => Discard.getBuilder
    case "ClampMTU" => ClampMTU.getBuilder
    case "DHCPSetState" => DHCPSetState.getBuilder
    case "DHCPCheckState" => DHCPCheckState.getBuilder
    case "IPRewriter"  => IPRewriter.getBuilder
    case "IPEncap" => IPEncap.getBuilder
    case "IPDecap" => IPDecap.getBuilder
    case "IPsecESPEncap" => IPsecESPEncap.getBuilder
    case "IPsecESPDecap" => IPsecESPDecap.getBuilder
    case "ICMPPingSource" => ICMPPingSource.getBuilder
    case "ICMPPingResponder" => ICMPPingResponder.getBuilder
    case "IPsecAES" => IPsecAES.getBuilder
    case "IPClassifier"  => IPClassifier.getBuilder
    case "StripIPHeader" => StripIPHeader.getBuilder
    case "CheckIPHeader" => CheckIPHeader.getBuilder
    case "IPMirror" => IPMirror.getBuilder
    case "DecIPTTL" => DecIPTTL.getBuilder
    case "EtherEncap" => EtherEncap.getBuilder
    case "EtherDecap" => EtherDecap.getBuilder
    case "VLANEncap" => VLANEncap.getBuilder
    case "VLANDecap" => VLANDecap.getBuilder
    case "HostEtherFilter" => HostEtherFilter.getBuilder
    case "Template"  => Template.getBuilder
    case "AddTCPOptions"  => AddTCPOptions.getBuilder
    case "ScanTCPOptions"  => ScanTCPOptions.getBuilder
//    case "IPFilter"  => IPFilter.getBuilder
    case "Paint"  => Paint.getBuilder
    case "CheckDHCPMsg"  => CheckDHCPMsg.getBuilder
    case "DHCPClassifier"  => DHCPClassifier.getBuilder
    case "DHCPClient"  => DHCPClient.getBuilder
    case "DHCPICMPEncap"  => DHCPICMPEncap.getBuilder
    case "DHCPLeaseHash"  => DHCPLeaseHash.getBuilder
    case "DHCPLeasePool"  => DHCPLeasePool.getBuilder
    case "DHCPServerACKorNAK"  => DHCPServerACKorNAK.getBuilder
    case "DHCPServerOffer"  => DHCPServerOffer.getBuilder
    case "DHCPServerRelease"  => DHCPServerRelease.getBuilder
    case "UDPIPEncap"  => UDPIPEncap.getBuilder
    case _ => NoOpClickElm.getBuilder(elementType)
  }
}

