unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ValEdit, ExtCtrls,
  PINGsend,
  BCPanel, FluentCheckControls,
  IdGlobal,
  IdStack,
  IdStackWindows,
  //IdStackLinux,
  //IdStackBSDBase,
  IdNetworkCalculator,
  IdIMAP4,
  //IdEcho,
  //IdMappedPortUDP,
  IdMappedFTP
  ;


type

  { TForm1 }

  TForm1 = class(TForm)
    BCPanel1: TBCPanel;
    ExitButton: TButton;
    FluentCheckBox1: TFluentCheckBox;
    GetLocalIPListButton: TButton;
    PingLocalIPListButton: TButton;
    IPEdit: TEdit;
    IPLabel: TLabel;
    IPLabel1: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Ping2Button: TButton;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    PingSend: TPINGSend;
    TabSheet2: TTabSheet;
    TimeoutEdit: TEdit;
    ValueListEditor1: TValueListEditor;
    ValueListEditor2: TValueListEditor;
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetLocalIPListButtonClick(Sender: TObject);
    procedure Ping2ButtonClick(Sender: TObject);
    procedure PingLocalIPListButtonClick(Sender: TObject);
    procedure ValueListEditor1Selection(Sender: TObject; aCol, aRow: Integer);
  private

  public

  end;


var
  Form1: TForm1;


implementation


{$R *.lfm}


{ TForm1 }

procedure TForm1.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PingSend := TPINGSend.Create;
  Memo1.Lines.Clear;
  ValueListEditor1.Clear;
  ValueListEditor2.Clear;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  PingSend.Free;
end;


procedure TForm1.GetLocalIPListButtonClick(Sender: TObject);
var
  I: Integer;
  AIPList: TStringList;
  IdStackLocalAddressList: TIdStackLocalAddressList;
begin
  ValueListEditor1.Clear;
  //TIdStackWindows.IncUsage;
  TIdStack.IncUsage;
  // Получаем список локальных IP-адресов
  AIPList := TStringList(GWindowsStack.LocalAddresses);
  //GWindowsStack.GetLocalAddressList(IdStackLocalAddressList);
  ValueListEditor1.RowCount := AIPList.Count;
  for i := 0 to AIPList.Count - 1 do
    begin
      ValueListEditor1.Keys[i + 1] := AIPList[i];
      ValueListEditor1.Values[ValueListEditor1.Keys[i + 1]] := '255.255.255.224';
      //ValueListEditor1.Values[ValueListEditor1.Keys[i + 1]] :=
      //IPStrings.Add(IPList[i].IPAddress + ':' + TIdStackLocalAddressIPv4(IPList[i]).SubNetMask);
    end;
  //TIdStackWindows.DecUsage;
  TIdStack.DecUsage;
end;


procedure TForm1.ValueListEditor1Selection(Sender: TObject; aCol, aRow: Integer);
// Формируем по маске пул IP-адресов
var
  AIdNetworkCalculator: TIdNetworkCalculator;
  AIPList: TStrings;
  I: Integer;
begin
  ValueListEditor2.Clear;
  AIdNetworkCalculator := TIdNetworkCalculator.Create;
  //AIPList := TStrings.Create;
  try
    AIdNetworkCalculator.NetworkAddress.AsString := ValueListEditor1.Keys[aRow];
    AIdNetworkCalculator.NetworkMask.AsString := ValueListEditor1.Values[ValueListEditor1.Keys[aRow]];
    AIPList := AIdNetworkCalculator.ListIP;
    ValueListEditor2.RowCount := AIPList.Count + 1;
    for i := 0 to AIPList.Count - 1 do
      begin
        ValueListEditor2.Keys[i + 1] := AIPList[i];
        ValueListEditor2.Values[AIPList[i]] := '-';
      end;
  finally
    //AIPList.Free;
    AIdNetworkCalculator.Free;
  end;
  PingLocalIPListButton.Enabled := True;
end;


procedure TForm1.PingLocalIPListButtonClick(Sender: TObject);
// Пингуем пул IP-адресов
var
  I: Integer;
begin
  try
    PingSend.Timeout := StrToInt(TimeoutEdit.Text);
  except
    PingSend.Timeout := 20;
  end;
  for i := 1 to ValueListEditor2.RowCount - 1 do
    begin
      if PingSend.Ping(ValueListEditor2.Keys[i]) then
        ValueListEditor2.Values[ValueListEditor2.Keys[i]] := 'Ok ' + IntToStr(PingSend.PingTime) + ' ms'
      else
        ValueListEditor2.Values[ValueListEditor2.Keys[i]] := 'Ne ok';
      ValueListEditor2.Invalidate;
    end;
end;


procedure TForm1.Ping2ButtonClick(Sender: TObject);
var
  sIP: string;
  I: Integer;
begin
  try
    PingSend.Timeout := StrToInt(TimeoutEdit.Text);
  except
    PingSend.Timeout := 20;
  end;
  sIP := '192.168.0.';
  for i := 1 to 100 do
    if PingSend.Ping(sIP + IntToStr(i)) = True then
      begin
        Memo1.Lines.Add('Reply from ' + sIP + IntToStr(i) + ' in: ' + IntToStr(PingSend.PingTime) + ' ms');
      end
    else
      begin
        Memo1.Lines.Add('No response from ' + sIP + IntToStr(i) + ' in: ' + IntToStr(PingSend.Timeout) + ' ms');
      end;
end;


function getLocalIP: string;
var
  IPList: TIdStackLocalAddressList;
  IPStrings: TStringList;
  i: Integer;
begin
  Result := '';
  try
    IPList := TIdStackLocalAddressList.Create;
    try
      TIdStack.IncUsage;
      try
        GStack.GetLocalAddressList(IPList);
      finally
        TIdStack.DecUsage;
      end;
      if IPList.Count > 0 then
        begin
          IPStrings := TStringList.Create;
          try
            for i := 0 to IPList.Count - 1 do
              if IPList[i].IPVersion = Id_IPv4 then
                IPStrings.Add(IPList[i].IPAddress + ':' + TIdStackLocalAddressIPv4(IPList[i]).SubNetMask);
            if IPStrings.Count > 0 then
              Result := IPStrings[0];
          finally
            IPStrings.Free;
          end;
        end;
    finally
      IPList.Free;
    end;
  except
    on E: Exception do
      begin
        Result := '';
      end;
  end;
end;


end.

(*
program ip_addr;
{$MODE OBJFPC}

uses libc;

function GetIPAddressOfInterface( if_name: ANSIString): ANSIString;
var
  ifr : ifreq;
  sock : longint;
  p:pChar;

begin
  Result:='0.0.0.0';
  strncpy( ifr.ifr_ifrn.ifrn_name, pChar(if_name), IF_NAMESIZE-1 );
  ifr.ifr_ifru.ifru_addr.sa_family := AF_INET;
  sock := socket(AF_INET, SOCK_DGRAM, IPPROTO_IP);
  if ( sock >= 0 ) then begin
    if ( ioctl( sock, SIOCGIFADDR, @ifr ) >= 0 ) then begin
      p:=inet_ntoa( ifr.ifr_ifru.ifru_addr.sin_addr );
      if ( p <> nil ) then Result :=  p;
    end;
    libc.__close(sock);
  end;
end;



var
  i:LongInt;
begin
  if (ParamCount>0)
  then for i:=1 to ParamCount
       do WriteLn(GetIPAddressOfInterface(ParamStr(i)))
  else WriteLn('Usage:', ParamStr(0), '<interface-name>')
end.
*)
