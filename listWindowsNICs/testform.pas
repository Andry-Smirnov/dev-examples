unit testForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  process,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,

  // Application Related
  netutils;

type

  { TForm1 }

  TForm1 = class (TForm)
    GetInfoButton: TButton;
    GetInfo2Button: TButton;
    ExitButton: TButton;
    LogMemo: TMemo;
    Panel1: TPanel;
    procedure GetInfo2ButtonClick(Sender: TObject);
    procedure GetInfoButtonClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.GetInfoButtonClick(Sender: TObject);
var
  i: integer;
  aNetInterfaceList: tNetworkInterfaceList;
begin
  try
    if GetNetworkInterfaces(aNetInterfaceList) then
      begin
        LogMemo.Clear;
        LogMemo.Lines.Add(DateTimeToStr(Now) + ' : ');
        //ShowMessage('Size of aNetInterfaceList - ' + IntToStr(High(aNetInterfaceList)));
        for i := 0 to High(aNetInterfaceList) do
          begin
            LogMemo.Lines.Add('');
            LogMemo.Lines.Add('#                          : ' + IntToStr(i));
            LogMemo.Lines.Add('Name                       : ' + aNetInterfaceList[i].ComputerName);
            LogMemo.Lines.Add('IP-Address                 : ' + aNetInterfaceList[i].AddrIP);
            LogMemo.Lines.Add('Subnet mask                : ' + aNetInterfaceList[i].SubnetMask);
            LogMemo.Lines.Add('Net address                : ' + aNetInterfaceList[i].AddrNet);
            LogMemo.Lines.Add('Limited broadcast address  : ' + aNetInterfaceList[i].AddrLimitedBroadcast);
            LogMemo.Lines.Add('Directed Broadcast address : ' + aNetInterfaceList[i].AddrDirectedBroadcast);
            LogMemo.Lines.Add('Interface up               : ' + BoolToStr(aNetInterfaceList[i].IsInterfaceUp, True));
            LogMemo.Lines.Add('Broadcast supported        : ' + BoolToStr(aNetInterfaceList[i].BroadcastSupport, True));
            LogMemo.Lines.Add('Loopback interface         : ' + BoolToStr(aNetInterfaceList[i].IsLoopback, True));
            LogMemo.Lines.Add('');
          end;
      end;
  except
    raise;
  end;
end;

procedure TForm1.GetInfo2ButtonClick(Sender: TObject);
const
  READ_BYTES = 2048;
var
  OutputLines: TStringList;
  MemStream: TMemoryStream;
  OurProcess: TProcess;
  NumBytes: LongInt;
  BytesRead: LongInt;
begin
  // A temp Memorystream is used to buffer the output
  MemStream := TMemoryStream.Create;
  BytesRead := 0;

  OurProcess := TProcess.Create(nil);

  OurProcess.Executable := 'ipconfig';
  OurProcess.Parameters.Add('/all');

  OurProcess.Options := [poUsePipes, poNoConsole];
  OurProcess.Execute;

  LogMemo.Clear;

  while True do
    begin
      // make sure we have room
      MemStream.SetSize(BytesRead + READ_BYTES);

      // try reading it
      NumBytes := OurProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
      // All read() calls will block, except the final one.
      if NumBytes > 0 then
        Inc(BytesRead, NumBytes)
      else
        // Program has finished execution.
        Break;
    end;
  //if BytesRead > 0 then WriteLn;
  MemStream.SetSize(BytesRead);

  OutputLines := TStringList.Create;
  OutputLines.LoadFromStream(MemStream);

  LogMemo.Lines.AddStrings(OutputLines);
  OutputLines.Free;
  OurProcess.Free;
  MemStream.Free;
end;

procedure TForm1.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

end.
