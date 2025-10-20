// *****************************************************************************
//   File    : UServer.pas
//   Project : MicroServer.dpr
//             Easy example of TCP Server with indy component : TidTCPSever

//   see indy doc: http://www.indyproject.org/sockets/docs/index.en.aspx


// *****************************************************************************
unit UServer;

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
  ExtCtrls,
  StdCtrls,
  IdContext,
  IdComponent,
  IdBaseComponent,
  IdCustomTCPServer,
  //IdThreadSafe,
  IdTCPConnection,
  //IdYarn,
  IdTCPServer
  ;


type
  TServerForm = class(TForm)
    LabelTitle: TLabel;

    StartServerButton: TButton;
    StopServerButton: TButton;
    ClearLogButton: TButton;

    clients_connected: TLabel;
    LabelClientsCount: TLabel;
    Panel1: TPanel;
    MessagesLog: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure StartServerButtonClick(Sender: TObject);
    procedure StopServerButtonClick(Sender: TObject);
    procedure ClearLogButtonClick(Sender: TObject);

    procedure IdTCPServerConnect(AContext: TIdContext);
    procedure IdTCPServerDisconnect(AContext: TIdContext);
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);

    procedure ShowNumberOfClients(p_disconnected: boolean = False);

    procedure BroadcastMessage(p_message: string);

    procedure Display(p_sender, p_message: string);
    function GetNow(): string;
  private
    FPSender: string;
    FPMessage: string;
    FNClients: Integer;
    // ... Id TCP Server
    FIdTCPServer: TIdTCPServer;

    procedure AddLogMessage;
    procedure SetServerCaption;
  public
  end;
  // ...


  // ... listening port
const
  GUEST_CLIENT_PORT = 20010;


var
  ServerForm: TServerForm;


implementation


{$R *.lfm}


// *****************************************************************************
//   EVENT : onCreate()
//           ON FORM CREATE
// *****************************************************************************
procedure TServerForm.FormCreate(Sender: TObject);
begin
  // ... create FIdTCPServer
  FIdTCPServer := TIdTCPServer.Create(self);
  FIdTCPServer.Active := False;

  // ... set properties
  FIdTCPServer.MaxConnections := 20;

  // ... etc..

  // ... assign a new context class (if you need)
  // FIdTCPServer.ContextClass    := TYourContext;

  // ... add some callback functions
  FIdTCPServer.OnConnect := IdTCPServerConnect;
  FIdTCPServer.OnDisconnect := IdTCPServerDisconnect;
  FIdTCPServer.OnExecute := IdTCPServerExecute;
  FIdTCPServer.OnStatus := IdTCPServerStatus;
  // ... etc..
end;


// *****************************************************************************
//   EVENT : onShow()
//           ON FORM SHOW
// *****************************************************************************
procedure TServerForm.FormShow(Sender: TObject);
begin
  // ... INITIALIZE:

  // ... clear message log
  MessagesLog.Lines.Clear;

  // ... zero to clients connected
  clients_connected.Caption := IntToStr(0);

  // ... set buttons
  StartServerButton.Enabled := True;
  StopServerButton.Enabled := False;
end;


// *****************************************************************************
//   EVENT : btn_startClick()
//           CLICK ON START BUTTON
// *****************************************************************************
procedure TServerForm.StartServerButtonClick(Sender: TObject);
begin
  // ... START SERVER:

  // ... clear the Bindings property ( ... Socket Handles )
  FIdTCPServer.Bindings.Clear;
  // ... Bindings is a property of class: TIdSocketHandles;

  // ... add listening ports:

  // ... add a port for connections from guest clients.
  FIdTCPServer.Bindings.Add.Port := GUEST_CLIENT_PORT;
  // ... etc..


  // ... ok, Active the Server!
  FIdTCPServer.Active := True;

  // ... disable start button
  StartServerButton.Enabled := False;

  // ... enable stop button
  StopServerButton.Enabled := True;

  // ... message log
  Display('SERVER', 'STARTED!');
end;


// *****************************************************************************
//   EVENT : btn_stopClick()
//           CLICK ON STOP BUTTON
// *****************************************************************************
procedure TServerForm.StopServerButtonClick(Sender: TObject);
begin
  // ... before stopping the server ... send 'good bye' to all clients connected
  BroadcastMessage('Goodbye Client ');

  // ... stop server!
  FIdTCPServer.Active := False;

  // ... hide stop button
  StopServerButton.Enabled := False;

  // ... show start button
  StartServerButton.Enabled := True;

  // ... message log
  Display('SERVER', 'STOPPED!');
end;


// *****************************************************************************
//   EVENT : btn_clearClick()
//           CLICK ON CLEAR BUTTON
// *****************************************************************************
procedure TServerForm.ClearLogButtonClick(Sender: TObject);
begin
  //... clear messages log
  MessagesLog.Lines.Clear;
end;

// .............................................................................
// .............................................................................
// .............................................................................

// *****************************************************************************
//   EVENT : onConnect()
//           OCCURS ANY TIME A CLIENT IS CONNECTED
// *****************************************************************************
procedure TServerForm.IdTCPServerConnect(AContext: TIdContext);
var
  //ip: string;
  port: integer;
  peerIP: string;
  peerPort: integer;

  //nClients: integer;

  msgToClient: string;
  typeClient: string;
begin
  // ... OnConnect is a TIdServerThreadEvent property that represents the event
  //     handler signalled when a new client connection is connected to the server.

  // ... Use OnConnect to perform actions for the client after it is connected
  //     and prior to execution in the OnExecute event handler.

  // ... see indy doc:
  //     http://www.indyproject.org/sockets/docs/index.en.aspx

  // ... getting IP address and Port of Client that connected
  //ip := AContext.Binding.IP;
  port := AContext.Binding.Port;
  peerIP := AContext.Binding.PeerIP;
  peerPort := AContext.Binding.PeerPort;

  // ... message log
  Display('SERVER', 'Client Connected!');
  Display('SERVER', 'Port=' + IntToStr(Port) +
    ' ' + '(PeerIP=' + PeerIP + ' - ' + 'PeerPort=' +
    IntToStr(PeerPort) + ')'
    );

  // ... display the number of clients connected
  ShowNumberOfClients();

  // ... CLIENT CONNECTED:
  case Port of
    GUEST_CLIENT_PORT:  begin
                          // ... GUEST CLIENTS
                          typeClient := 'GUEST';
                        end;
    // ...
    else
      typeClient := '';
  end;

  // ... send the Welcome message to Client connected
  msgToClient := 'Welcome ' + typeClient + ' ' + 'Client :)';
  AContext.Connection.IOHandler.WriteLn(msgToClient);
end;

// *****************************************************************************
//   EVENT : onDisconnect()
//           OCCURS ANY TIME A CLIENT IS DISCONNECTED
// *****************************************************************************
procedure TServerForm.IdTCPServerDisconnect(AContext: TIdContext);
var
  //ip: string;
  //port: integer;
  peerIP: string;
  peerPort: integer;

  //nClients: integer;
begin
  // ... getting IP address and Port of Client that connected
  //ip := AContext.Binding.IP;
  //port := AContext.Binding.Port;
  peerIP := AContext.Binding.PeerIP;
  peerPort := AContext.Binding.PeerPort;

  // ... message log
  Display('SERVER', 'Client Disconnected! Peer=' + PeerIP + ':' + IntToStr(PeerPort));

  // ... display the number of clients connected
  ShowNumberOfClients(True);
end;


// *****************************************************************************
//   EVENT : onExecute()
//           ON EXECUTE THREAD CLIENT
// *****************************************************************************
procedure TServerForm.IdTCPServerExecute(AContext: TIdContext);
var
  //Port: integer;
  PeerPort: integer;
  PeerIP: string;

  msgFromClient: string;
  //msgToClient: string;
begin

  // ... OnExecute is a TIdServerThreadEvents event handler used to execute
  //     the task for a client connection to the server.

  // ... here you can check connection status and buffering before reading
  //     messages from client

  // ... see doc:
  // ... AContext.Connection.IOHandler.InputBufferIsEmpty
  // ... AContext.Connection.IOHandler.CheckForDataOnSource(<milliseconds>);
  //     (milliseconds to wait for the connection to become readable)
  // ... AContext.Connection.IOHandler.CheckForDisconnect;

  // ... received a message from the client

  // ... get message from client
  msgFromClient := AContext.Connection.IOHandler.ReadLn;

  // ... getting IP address, Port and PeerPort from Client that connected
  peerIP := AContext.Binding.PeerIP;
  peerPort := AContext.Binding.PeerPort;

  // ... message log
  Display('CLIENT', '(Peer=' + PeerIP + ':' + IntToStr(PeerPort) +
    ') ' + msgFromClient);
  // ...

  // ... process message from Client

  // ...

  // ... send response to Client

  AContext.Connection.IOHandler.WriteLn('... message sent from server :)');
end;


// *****************************************************************************
//   EVENT : onStatus()
//           ON STATUS CONNECTION
// *****************************************************************************
procedure TServerForm.IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  // ... OnStatus is a TIdStatusEvent property that represents the event handler
  //     triggered when the current connection state is changed...

  // ... message log
  Display('SERVER', AStatusText);
end;


// *****************************************************************************
//   FUNCTION : getNow()
//              GET MOW DATE TIME
// *****************************************************************************
function TServerForm.getNow(): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ';
end;


// *****************************************************************************
//   PROCEDURE : broadcastMessage()
//               BROADCAST A MESSAGE TO ALL CLIENTS CONNECTED
// *****************************************************************************
procedure TServerForm.broadcastMessage(p_message: string);
var
  tmpList: TList;
  contexClient: TidContext;
  //nClients: integer;
  i: integer;
begin

  // ... send a message to all clients connected

  // ... get context Locklist
  tmpList := FIdTCPServer.Contexts.LockList;

  try
    i := 0;
    while (i < tmpList.Count) do
      begin
        // ... get context (thread of i-client)
        contexClient := tmpList[i];

        // ... send message to client
        contexClient.Connection.IOHandler.WriteLn(p_message);
        i += 1;
      end;
  finally
    // ... unlock list of clients!
    FIdTCPServer.Contexts.UnlockList;
  end;
end;


procedure TServerForm.AddLogMessage;
begin
  MessagesLog.Lines.Add('[' + FPSender + '] - ' + GetNow() + ': ' + FPMessage);
end;


// *****************************************************************************
//   PROCEDURE : Display()
//               DISPLAY MESSAGE UPON SYSOUT
// *****************************************************************************
procedure TServerForm.Display(p_sender: string; p_message: string);
begin
  // ... DISPLAY MESSAGE
  FPSender := p_sender;
  FPMessage := p_message;
  TThread.Queue(nil, AddLogMessage//(p_sender, p_message)
    //procedure
    //begin
      //MessagesLog.Lines.Add('[' + p_sender +
      //'] - ' + getNow() + ': ' + p_message);
    //end
    );

  // ... see doc..
  // ... TThread.Queue() causes the call specified by AMethod to
  //     be asynchronously executed using the main thread, thereby avoiding
  //     multi-thread conflicts.
end;


procedure TServerForm.SetServerCaption;
begin
  clients_connected.Caption := IntToStr(FNClients);
end;


// *****************************************************************************
//   PROCEDURE : ShowNumberOfClients()
//               NUMBER OF CLIENTS CONNECTD
// *****************************************************************************
procedure TServerForm.ShowNumberOfClients(p_disconnected: boolean = False);
//var
  //nClients: integer;
begin
  try
    // ... get number of clients connected
    //nClients := FIdTCPServer.Contexts.LockList.Count;
    FNClients := FIdTCPServer.Contexts.LockList.Count;
  finally
    FIdTCPServer.Contexts.UnlockList;
  end;

  // ... client disconnected?
  if p_disconnected then
    //Dec(nClients);
    Dec(FNClients);

  // ... display
  TThread.Queue(nil, SetServerCaption
    //procedure
    //begin
      //clients_connected.Caption := IntToStr(nClients);
    //end
    );
end;


end.
