unit Main;

{$MODE Delphi}


{$I reports-config.inc}


interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frxDesgn,
  frxPreview, frxClass, StdCtrls;


type
  TMainForm = class(TForm)
    ExitButton: TButton;
    ReportButton: TButton;
    ShowFormButton: TButton;
    procedure ExitButtonClick(Sender: TObject);
    procedure ReportButtonClick(Sender: TObject);
    procedure ShowFormButtonClick(Sender: TObject);
  end;


var
  MainForm: TMainForm;


implementation


uses
  EEPROMreport,
  SHUK_PT_MBParams
  ;

{$R *.lfm}

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ReportButtonClick(Sender: TObject);
begin
  frmEEPROMreport.ShowReport(frmSHUK_PT_MBParams);
end;

procedure TMainForm.ShowFormButtonClick(Sender: TObject);
begin
  frmSHUK_PT_MBParams.Show;
end;

end.
