{$CODEPAGE UTF8}
{
  @abstract(Генератор отчетов SCADA EISA)
  @author(Andry Smirnov)
  @created(????)
  @lastmod(08.06.2026)

  ********************************* History  ***********************************
  ******************************************************************************
  08.06.2026 Конвертировал в Lazarus.
  @author(Andry Smirnov)
  ******************************************************************************
}
program EISAReports;


{$IFDEF WINDOWS}
  {$APPTYPE GUI}
{$ENDIF}
{%ToDo 'EISAReports.todo'}
{%File 'reports-config.inc'}
{$I reports-config.inc}
{$IFDEF ShowCompiledUnitCaption}
  {$HINT Compile scadaeisa}
{$ENDIF}


{$IFOPT D+}
  {$NOTE debug mode is active}
{$ENDIF}


uses
{$IFDEF UNIX}
  // Рекомендуемая последовательность объявления модулей
  // См. https://wiki.lazarus.freepascal.org/Multithreaded_Application_Tutorial/ru
  cthreads,
  cmem, // менеджер памяти Си в некоторых системах намного быстрее для многопоточности
  cwstring,
  //BaseUnix,
  //unixtype,
  //heaptrc,
{$ENDIF}
{$IFDEF HASAMIGA}
  athreads,
{$ENDIF}
  // Вы не можете использовать ключ -gh с модулем cmem. Ключ -gh использует
  // модуль heaptrc, который расширяет менеджер кучи. Поэтому модуль heaptrc
  // должен указываться после модуля cmem
  // См. https://wiki.lazarus.freepascal.org/Multithreaded_Application_Tutorial/ru
  //heaptrc,
  FPCAdds,
  LazUTF8,
  Interfaces, // this includes the LCL widgetset
  Forms,
  //
  Main in 'Main.pas' {frmMain},
  EEPROMreport in 'EEPROMreport.pas' {EEPROMReportForm},
  SHUK_PT_MBParams in 'SHUK_PT_MBParams.pas' {frmSHUK_PT_MBParams}
  ;


{$R *.res}


begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEEPROMReportForm, EEPROMReportForm);
  Application.CreateForm(TfrmSHUK_PT_MBParams, frmSHUK_PT_MBParams);
  Application.Run;
end.
