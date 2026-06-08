program EISAReports;

{$MODE Delphi}

{%ToDo 'EISAReports.todo'}
{%File 'reports-config.inc'}
{$I reports-config.inc}

uses
  ExceptionLog,
  Forms, Interfaces,
  Main in 'Main.pas' {frmMain},
  EEPROMreport in 'EEPROMreport.pas' {EEPROMReportForm},
  SHUK_PT_MBParams in 'SHUK_PT_MBParams.pas' {frmSHUK_PT_MBParams};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEEPROMReportForm, EEPROMReportForm);
  Application.CreateForm(TfrmSHUK_PT_MBParams, frmSHUK_PT_MBParams);
  Application.Run;
end.
