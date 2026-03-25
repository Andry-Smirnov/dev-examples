unit Main;

{$I reports-config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frxDesgn, frxPreview, frxClass, StdCtrls;

type
  TfrmMain = class(TForm)
    btnExit: TButton;
    btnReport: TButton;
    btn1: TButton;
    procedure btnExitClick(Sender: TObject);
    procedure btnReportClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses
  EEPROMreport,
  SLAParamReg,
  SHUK_PT_MBParams;

{$R *.dfm}

procedure TfrmMain.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnReportClick(Sender: TObject);
begin
  frmEEPROMreport.ShowReport(frmSHUK_PT_MBParams);
end;

procedure TfrmMain.btn1Click(Sender: TObject);
begin
  frmSHUK_PT_MBParams.Show;
end;

end.
