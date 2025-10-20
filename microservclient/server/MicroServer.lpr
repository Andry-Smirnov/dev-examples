program MicroServer;

{$MODE Delphi}

uses
  Forms,
  indylaz,
  Interfaces,
  UServer in 'UServer.pas' {FServer};

{$R *.res}

begin
  Application.Initialize;
  //Application.MainFormOnTaskbar := True;
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
