program project1;

{$mode objfpc}{$H+}
{$codepage utf8}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
{$IFDEF HASAMIGA}
  athreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  etpackage,
  pascalscript, lazcontrols,
  uecontrols,
  rxnew,
  { you can add units after this }
  mainunit, digitalinput
  ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDigitalInputForm, DigitalInputForm);
  Application.Run;
end.

