// *****************************************************************************
//   File    : UClient.pas
//   Project : MicroServer.dpr
//             Easy example of TCP Client with indy component: TidTCPClient

//   see indy doc: http://www.indyproject.org/sockets/docs/index.en.aspx

// *****************************************************************************
unit UClient;

{$MODE Delphi}

interface

uses
  //Windows,
  //Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdThreadComponent;

type
  TClientForm = class(TForm)
    LabelClient: TLabel;
    LabelMessage: TLabel;
    MessageToSend: TMemo;
    MessagesLog: TMemo;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    SendMessageButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SendMessageButtonClick(Sender: TObject);
    procedure IdTCPClientConnected(Sender: TObject);
    procedure IdTCPClientDisconnected(Sender: TObject);
    procedure IdThreadComponentRun(Sender: TIdThreadComponent);
    procedure Display(p_sender: string; p_message: string);
    function GetNow(): string;
  private
    FPSender: string;
    FPMessage: string;
    FTCPClient: TIdTCPClient;
    FThreadComponent: TIdThreadComponent;

    procedure AddLogMessage;
  public
  end;

  // listening port : GUEST CLIENT
const
  GUEST_PORT = 20010;


var
  ClientForm: TClientForm;


implementation


{$R *.lfm}


// *****************************************************************************
//   EVENT : onCreate()
//           ON CREATE FORM
// *****************************************************************************
procedure TFClient.FormCreate(Sender: TObject);
begin
  // ... create TIdTCPClient
  FTCPClient := TIdTCPClient.Create();

  // ... set properties
  FTCPClient.Host := 'localhost';
  FTCPClient.Port := GUEST_PORT;
  // ... etc..

  // ... callback functions
  FTCPClient.OnConnected := IdTCPClientConnected;
  FTCPClient.OnDisconnected := IdTCPClientDisconnected;
  // ... etc..

  // ... create TIdThreadComponent
  FThreadComponent := TIdThreadComponent.Create();

  // ... callback functions
  FThreadComponent.OnRun := IdThreadComponentRun;
  // ... etc..
end;
// .............................................................................


// *****************************************************************************
//   EVENT : onShow()
//           ON SHOW FORM
// *****************************************************************************
procedure TFClient.FormShow(Sender: TObject);
begin
  // ... INITAILIZE

  // ... message to send
  MessageToSend.Clear;
  MessageToSend.Enabled := False;

  // ... clear log
  MessagesLog.Clear;

  // ... buttons
  ConnectButton.Enabled := True;
  DisconnectButton.Enabled := False;
  SendMessageButton.Enabled := False;
end;
// .............................................................................


// *****************************************************************************
//   EVENT : btn_connectClick()
//           CLICK ON CONNECT BUTTON
// *****************************************************************************
procedure TFClient.ConnectButtonClick(Sender: TObject);
begin
  // ... disable connect button
  ConnectButton.Enabled := False;

  // ... try to connect to Server
  try
    FTCPClient.Connect;
  except
    on E: Exception do
    begin
      Display('CLIENT', 'CONNECTION ERROR! ' + E.Message);
      ConnectButton.Enabled := True;
    end;
  end;
end;
// .............................................................................


// *****************************************************************************
//   EVENT : btn_disconnectClick()
//           CLICK ON DISCONNECT BUTTON
// *****************************************************************************
procedure TFClient.DisconnectButtonClick(Sender: TObject);
begin
  // ... is connected?
  if FTCPClient.Connected then
  begin
    // ... disconnect from Server
    FTCPClient.Disconnect;

    // ... set buttons
    ConnectButton.Enabled := True;
    DisconnectButton.Enabled := False;
    SendMessageButton.Enabled := False;
    MessageToSend.Enabled := False;
  end;
end;
// .............................................................................


// *****************************************************************************
//   EVENT : onConnected()
//           OCCURS WHEN A CLIENT IS CONNETED
// *****************************************************************************
procedure TFClient.IdTCPClientConnected(Sender: TObject);
begin
  // ... messages log
  Display('CLIENT', 'CONNECTED!');

  // ... after connection is ok, run the Thread ... waiting messages from server
  FThreadComponent.Active := True;

  // ... set buttons
  ConnectButton.Enabled := False;
  DisconnectButton.Enabled := True;
  SendMessageButton.Enabled := True;

  // ... enable message to send
  MessageToSend.Enabled := True;
end;
// .............................................................................


// *****************************************************************************
//   EVENT : onDisconnected()
//           OCCURS WHEN CLIENT IS DISCONNECTED
// *****************************************************************************
procedure TFClient.IdTCPClientDisconnected(Sender: TObject);
begin
  // ... message log
  Display('CLIENT', 'DISCONNECTED!');
end;
// .............................................................................


// *****************************************************************************
//   EVENT : btn_sendClick()
//           CLICK ON SEND BUTTON
// *****************************************************************************
procedure TFClient.SendMessageButtonClick(Sender: TObject);
begin
  // ... send message to Server
  FTCPClient.IOHandler.WriteLn(MessageToSend.Text);
end;
// .............................................................................


// *****************************************************************************
//   EVENT : onRun()
//           OCCURS WHEN THE SERVER SEND A MESSAGE TO CLIENT
// *****************************************************************************
procedure TFClient.IdThreadComponentRun(Sender: TIdThreadComponent);
var
  MsgFromServer: string;
begin
  // ... read message from server
  MsgFromServer := FTCPClient.IOHandler.ReadLn();

  // ... messages log
  Display('SERVER', MsgFromServer);
end;
// .............................................................................


procedure TFClient.AddLogMessage;
begin
  MessagesLog.Lines.Add('[' + FPSender + '] - ' + GetNow() + ': ' + FPMessage);
end;


// *****************************************************************************
//   PROCEDURE : Display()
//               DISPLAY MESSAGE UPON SYSOUT
// *****************************************************************************
procedure TFClient.Display(p_sender: string; p_message: string);
begin
  FPSender := p_sender;
  FPMessage := p_message;
  TThread.Queue(nil, AddLogMessage
    //procedure
      //begin
        //MessagesLog.Lines.Add('[' + p_sender +
        //'] - ' + GetNow() + ': ' + p_message);
      //end
    );
end;
// .............................................................................


// *****************************************************************************
//   FUNCTION : getNow()
//              GET MOW DATE TIME
// *****************************************************************************
function TFClient.getNow(): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ';
end;
// .............................................................................


// *****************************************************************************
//   EVENT : FormClose()
//           ON FORM CLOSE
// *****************************************************************************
procedure TFClient.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // ... terminate thread
  if FThreadComponent.Active then
    FThreadComponent.Active := False;

  // ... free
  FreeAndNil(FTCPClient);
  FreeAndNil(FThreadComponent);
end;


end.
