program MicroClient;

{$MODE Delphi}

uses
  Forms,
  indylaz,
  Interfaces,
  UClient in 'UClient.pas' {FClient};

{$R *.res}

begin
  Application.Initialize;
  //Application.MainFormOnTaskbar := True;
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
