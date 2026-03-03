program keyboardstest;


{$IFDEF WINDOWS}
  {$APPTYPE GUI}
{$ENDIF}
{$MODE objfpc}
{$H+}


uses
{$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  keytest,
  fullkeyboard
  ;


{$R *.res}


begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(Tfullkeyboardform, fullkeyboardform);
  Application.Run;
end.

