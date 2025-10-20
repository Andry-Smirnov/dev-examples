unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  TplMaskEditUnit,
  JvLED,
  IdTCPClient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Client3GroupBox: TGroupBox;
    Client4GroupBox: TGroupBox;
    Connect1Button: TButton;
    Connect3Button: TButton;
    Connect2Button: TButton;
    Connect4Button: TButton;
    ConnectButton: TButton;
    Disconnect1Button: TButton;
    Disconnect3Button: TButton;
    Disconnect2Button: TButton;
    Disconnect4Button: TButton;
    DisconnectButton: TButton;
    ConnectEdit: TEdit;
    Client1GroupBox: TGroupBox;
    Client2GroupBox: TGroupBox;
    IdTCPClient3: TIdTCPClient;
    IdTCPClient4: TIdTCPClient;
    IP3Label: TLabel;
    IP3MaskEdit: TplMaskEdit;
    IP2Label: TLabel;
    IP4Label: TLabel;
    IP2MaskEdit: TplMaskEdit;
    IP4MaskEdit: TplMaskEdit;
    JvLED1: TJvLED;
    JvLED2: TJvLED;
    JvLED3: TJvLED;
    JvLED4: TJvLED;
    Client1Label: TLabel;
    Client2Label: TLabel;
    IP1Label: TLabel;
    Client3Label: TLabel;
    Client4Label: TLabel;
    Port1Label: TLabel;
    IP1MaskEdit: TplMaskEdit;
    Port3Label: TLabel;
    Port3MaskEdit: TplMaskEdit;
    Port2Label: TLabel;
    Port1MaskEdit: TplMaskEdit;
    Port4Label: TLabel;
    Port2MaskEdit: TplMaskEdit;
    Port4MaskEdit: TplMaskEdit;
    ReadEdit: TEdit;
    ExitButton: TButton;
    GroupBox1: TGroupBox;
    IdTCPClient1: TIdTCPClient;
    IdTCPClient2: TIdTCPClient;
    ConnectTimeoutLabel: TLabel;
    ReadTimeoutLabel: TLabel;
    UpdateInfoTimer: TTimer;
    procedure Connect3ButtonClick(Sender: TObject);
    procedure Connect4ButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure Connect1ButtonClick(Sender: TObject);
    procedure Connect2ButtonClick(Sender: TObject);
    procedure ConnectEditChange(Sender: TObject);
    procedure Disconnect3ButtonClick(Sender: TObject);
    procedure Disconnect4ButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure Disconnect1ButtonClick(Sender: TObject);
    procedure Disconnect2ButtonClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IP1MaskEditChange(Sender: TObject);
    procedure IP2MaskEditChange(Sender: TObject);
    procedure IP3MaskEditChange(Sender: TObject);
    procedure IP4MaskEditChange(Sender: TObject);
    procedure Port1MaskEditChange(Sender: TObject);
    procedure Port2MaskEditChange(Sender: TObject);
    procedure Port3MaskEditChange(Sender: TObject);
    procedure Port4MaskEditChange(Sender: TObject);
    procedure ReadEditChange(Sender: TObject);
    procedure UpdateInfoTimerTimer(Sender: TObject);
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
  ConnectEdit.Text := IntToStr(IdTCPClient1.ConnectTimeout);
  ReadEdit.Text := IntToStr(IdTCPClient1.ReadTimeout);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  IP1MaskEdit.Text := IdTCPClient1.Host;
  IP2MaskEdit.Text := IdTCPClient2.Host;
  IP3MaskEdit.Text := IdTCPClient3.Host;
  IP4MaskEdit.Text := IdTCPClient4.Host;
  Port1MaskEdit.Text := IntToStr(IdTCPClient1.Port);
  Port2MaskEdit.Text := IntToStr(IdTCPClient2.Port);
  Port3MaskEdit.Text := IntToStr(IdTCPClient3.Port);
  Port4MaskEdit.Text := IntToStr(IdTCPClient4.Port);
end;


procedure TForm1.IP1MaskEditChange(Sender: TObject);
begin
  try
    IdTCPClient1.Host := IP1MaskEdit.Text;
  except
  end;
end;


procedure TForm1.IP2MaskEditChange(Sender: TObject);
begin
  try
    IdTCPClient2.Host := IP2MaskEdit.Text;
  except
  end;
end;


procedure TForm1.IP3MaskEditChange(Sender: TObject);
begin
  try
    IdTCPClient3.Host := IP3MaskEdit.Text;
  except
  end;
end;

procedure TForm1.IP4MaskEditChange(Sender: TObject);
begin
  try
    IdTCPClient4.Host := IP4MaskEdit.Text;
  except
  end;
end;


procedure TForm1.Port1MaskEditChange(Sender: TObject);
begin
  try
    IdTCPClient1.Port := StrToInt(Port1MaskEdit.Text);
  except
  end;
end;


procedure TForm1.Port2MaskEditChange(Sender: TObject);
begin
  try
    IdTCPClient2.Port := StrToInt(Port2MaskEdit.Text);
  except
  end;
end;


procedure TForm1.Port3MaskEditChange(Sender: TObject);
begin
  try
    IdTCPClient3.Port := StrToInt(Port3MaskEdit.Text);
  except
  end;
end;


procedure TForm1.Port4MaskEditChange(Sender: TObject);
begin
  try
    IdTCPClient4.Port := StrToInt(Port4MaskEdit.Text);
  except
  end;
end;


procedure TForm1.ReadEditChange(Sender: TObject);
begin
 try
   IdTCPClient1.ReadTimeout := StrToInt(ReadEdit.Text);
   IdTCPClient2.ReadTimeout := StrToInt(ReadEdit.Text);
   IdTCPClient3.ReadTimeout := StrToInt(ReadEdit.Text);
   IdTCPClient4.ReadTimeout := StrToInt(ReadEdit.Text);
 except
 end;
end;


procedure TForm1.UpdateInfoTimerTimer(Sender: TObject);
begin
  JvLED1.Status := IdTCPClient1.Connected;
  JvLED2.Status := IdTCPClient2.Connected;
  JvLED3.Status := IdTCPClient3.Connected;
  JvLED4.Status := IdTCPClient4.Connected;
end;


procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  try
    if not IdTCPClient1.Connected then
      IdTCPClient1.Connect;
  except
  end;
  try
    if not IdTCPClient2.Connected then
      IdTCPClient2.Connect;
  except
  end;
  try
    if not IdTCPClient3.Connected then
      IdTCPClient3.Connect;
  except
  end;
  try
    if not IdTCPClient4.Connected then
      IdTCPClient4.Connect;
  except
  end;
end;


procedure TForm1.Connect1ButtonClick(Sender: TObject);
begin
  try
    if not IdTCPClient1.Connected then
      IdTCPClient1.Connect;
  except
  end;
end;


procedure TForm1.Connect2ButtonClick(Sender: TObject);
begin
  try
    if not IdTCPClient2.Connected then
      IdTCPClient2.Connect;
  except
  end;
end;


procedure TForm1.Connect3ButtonClick(Sender: TObject);
begin
  try
    if not IdTCPClient3.Connected then
      IdTCPClient3.Connect;
  except
  end;
end;


procedure TForm1.Connect4ButtonClick(Sender: TObject);
begin
  try
    if not IdTCPClient4.Connected then
      IdTCPClient4.Connect;
  except
  end;
end;


procedure TForm1.ConnectEditChange(Sender: TObject);
begin
  try
    IdTCPClient1.ConnectTimeout := StrToInt(ConnectEdit.Text);
  except
  end;
  try
    IdTCPClient2.ConnectTimeout := StrToInt(ConnectEdit.Text);
  except
  end;
  try
    IdTCPClient3.ConnectTimeout := StrToInt(ConnectEdit.Text);
  except
  end;
  try
    IdTCPClient4.ConnectTimeout := StrToInt(ConnectEdit.Text);
  except
  end;
end;

procedure TForm1.Disconnect1ButtonClick(Sender: TObject);
begin
  try
    IdTCPClient1.Disconnect;
  except
  end;
end;


procedure TForm1.Disconnect2ButtonClick(Sender: TObject);
begin
  try
    IdTCPClient2.Disconnect;
  except
  end;
end;


procedure TForm1.Disconnect3ButtonClick(Sender: TObject);
begin
  try
    IdTCPClient3.Disconnect;
  except
  end;
end;

procedure TForm1.Disconnect4ButtonClick(Sender: TObject);
begin
  try
    IdTCPClient4.Disconnect;
  except
  end;
end;


procedure TForm1.DisconnectButtonClick(Sender: TObject);
begin
  try
    IdTCPClient1.Disconnect;
  except
  end;
  try
    IdTCPClient2.Disconnect;
  except
  end;
  try
    IdTCPClient3.Disconnect;
  except
  end;
  try
    IdTCPClient4.Disconnect;
  except
  end;
end;


end.

