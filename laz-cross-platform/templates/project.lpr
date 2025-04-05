program project;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  MainForm;

{$IFDEF MSWINDOWS}
  {$R *.res}
{$ENDIF}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainFrm);
  Application.Run;
end.
