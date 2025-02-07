unit netUtils;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  Winsock
  ;


{ Unit to identify the network interfaces
  This code requires at least Win98/ME/2K, 95 OSR 2 or NT service pack #3
  as WinSock 2 is used (WS2_32.DLL) }


  // Constants found in manual on non-officially documented M$ Winsock functions
const
  SIO_GET_INTERFACE_LIST = $4004747F;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;


type
  SockAddr_Gen = packed record
    AddressIn: SockAddr_In;
    Padding: packed array [0..7] of byte;
  end;

  Interface_Info = record
    iiFlags: u_Long;
    iiAddress: SockAddr_Gen;
    iiBroadcastAddress: SockAddr_Gen;
    iiNetmask: SockAddr_Gen;
  end;

  tNetworkInterface = record
    ComputerName: string;
    AddrIP: string;
    SubnetMask: string;
    AddrNet: string;
    AddrLimitedBroadcast: string;
    AddrDirectedBroadcast: string;
    IsInterfaceUp: boolean;
    BroadcastSupport: boolean;
    IsLoopback: boolean;
  end;

  tNetworkInterfaceList = array of tNetworkInterface;


function WSAIoctl(aSocket: TSocket; aCommand: DWord; lpInBuffer: Pointer; dwInBufferLen: DWord; lpOutBuffer: Pointer; dwOutBufferLen: DWord; lpdwOutBytesReturned: LPDWord; lpOverLapped: Pointer; lpOverLappedRoutine: Pointer): integer; stdcall; external 'WS2_32.DLL';
// Returns a complete list the of available network interfaces on a system (IPv4)
function GetNetworkInterfaces(var aNetworkInterfaceList: tNetworkInterfaceList): boolean;


implementation


function GetNetworkInterfaces(var aNetworkInterfaceList: tNetworkInterfaceList): boolean;
  // Returns a complete list the of available network interfaces on a system (IPv4)
  // Copyright by Dr. Jan Schulz, 23-26th March 2007
  // This version can be used for free and non-profit projects. In any other case get in contact
  // Written with information retrieved from MSDN
  // www.code10.net
var
  aSocket: TSocket;
  aWSADataRecord: WSAData;
  NoOfInterfaces: integer;
  NoOfBytesReturned: u_Long;
  InterfaceFlags: u_Long;
  NameLength: DWord;
  pAddrIP: SockAddr_In;
  pAddrSubnetMask: SockAddr_In;
  pAddrBroadcast: Sockaddr_In;
  DirBroadcastDummy: In_Addr;
  NetAddrDummy: In_Addr;
  Buffer: array [0..30] of Interface_Info;
  i: integer;
begin
  Result := False;
  SetLength(aNetworkInterfaceList, 0);

  // Startup of old the WinSock
  // WSAStartup ($0101, aWSADataRecord);

  // Startup of WinSock2
  WSAStartup(MAKEWORD(2, 0), aWSADataRecord);

  // Open a socket
  aSocket := Socket(AF_INET, SOCK_STREAM, 0);

  // If impossible to open a socket, not worthy to go any further
  if (aSocket = INVALID_SOCKET) then
    Exit;

  try
    if WSAIoCtl(aSocket, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, @NoOfBytesReturned, nil, nil) <> SOCKET_ERROR then
    begin
      NoOfInterfaces := NoOfBytesReturned div SizeOf(Interface_Info);
      SetLength(aNetworkInterfaceList, NoOfInterfaces);

      // For each of the identified interfaces get:
      for i := 0 to NoOfInterfaces - 1 do
        with aNetworkInterfaceList[i] do
        begin
          // Get the name of the machine
          NameLength := MAX_COMPUTERNAME_LENGTH + 1;
          SetLength(ComputerName, NameLength);
          if not GetComputerName(PChar(Computername), NameLength) then
            ComputerName := '';

          // Get the IP address
          pAddrIP := Buffer[i].iiAddress.AddressIn;
          AddrIP := string(inet_ntoa(pAddrIP.Sin_Addr));

          // Get the subnet mask
          pAddrSubnetMask := Buffer[i].iiNetMask.AddressIn;
          SubnetMask := string(inet_ntoa(pAddrSubnetMask.Sin_Addr));

          // Get the limited broadcast address
          pAddrBroadcast := Buffer[i].iiBroadCastAddress.AddressIn;
          AddrLimitedBroadcast := string(inet_ntoa(pAddrBroadcast.Sin_Addr));

          // Calculate the net and the directed broadcast address
          NetAddrDummy.S_addr := Buffer[i].iiAddress.AddressIn.Sin_Addr.S_Addr;
          NetAddrDummy.S_addr := NetAddrDummy.S_addr and Buffer[i].iiNetMask.AddressIn.Sin_Addr.S_Addr;
          DirBroadcastDummy.S_addr := NetAddrDummy.S_addr or not Buffer[i].iiNetMask.AddressIn.Sin_Addr.S_Addr;

          AddrNet := string(inet_ntoa((NetAddrDummy)));
          AddrDirectedBroadcast := string(inet_ntoa((DirBroadcastDummy)));

          // From the evaluation of the Flags we receive more information
          InterfaceFlags := Buffer[i].iiFlags;

          // Is the network interface up or down ?
          if (InterfaceFlags and IFF_UP) = IFF_UP then
            IsInterfaceUp := True
          else
            IsInterfaceUp := False;

          // Does the network interface support limited broadcasts ?
          if (InterfaceFlags and IFF_BROADCAST) = IFF_BROADCAST then
            BroadcastSupport := True
          else
            BroadcastSupport := False;

          // Is the network interface a loopback interface ?
          if (InterfaceFlags and IFF_LOOPBACK) = IFF_LOOPBACK then
            IsLoopback := True
          else
            IsLoopback := False;
        end;
    end;
  except
    //Result := False;
  end;

  // Cleanup the mess
  CloseSocket(aSocket);
  WSACleanUp;
  Result := True;
end;

end.
