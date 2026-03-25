program EISAReports;

{%ToDo 'EISAReports.todo'}
{%File 'reports-config.inc'}
{$I reports-config.inc}

uses
  ExceptionLog,
  Forms,
  Main in 'Main.pas' {frmMain},
  EEPROMreport in 'EEPROMreport.pas' {frmEEPROMreport},
  SHUK_PT_MBParams in 'SHUK_PT_MBParams.pas' {frmSHUK_PT_MBParams};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmEEPROMreport, frmEEPROMreport);
  Application.CreateForm(TfrmSHUK_PT_MBParams, frmSHUK_PT_MBParams);
  Application.Run;
end.
