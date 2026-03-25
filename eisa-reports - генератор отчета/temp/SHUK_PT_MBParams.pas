//************************************************************
//
//  Модуль настройки регуляторов ШУК-ПТ Modbus
//
//  Разработчик: Шехтер Евгений Борисович
//  Дата:        Сентябрь 2013 г.
//
//************************************************************
unit SHUK_PT_MBParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, SYButton, ExtCtrls, Gauges,
  YSBitParamReg, YSSmallParamReg, YSParamReg, Printers,
  YSRealParamReg, RzTabs, RzStatus, RzPanel, SLAParamRegScrl,
  SLAParamRegOpt, SLAParamRegNum, SLAParamRegByte, SLAParamRegTemp;

type
  TfrmSHUK_PT_MBParams = class( TForm )
    RFlashTimer: TTimer;
    sdParams: TSaveDialog;
    odParams: TOpenDialog;
    TopPanel: TPanel;
    btnClose: TSYButton;
    pcParamWindows: TRzPageControl;
    NextWindowButton: TSYButton;
    RzGroupBox1: TRzGroupBox;
    LabelRd: TLabel;
    RPercent: TRzProgressStatus;
    RGauge: TRzProgressStatus;
    WPercent: TRzProgressStatus;
    RzGroupBox2: TRzGroupBox;
    PrintButton: TSYButton;
    btnWrite: TSYButton;
    CopyBuffButton: TSYButton;
    PasteBuffButton: TSYButton;
    btnSaveParam: TSYButton;
    btnLoadParam: TSYButton;
    LabelWr: TLabel;
    RzGroupBox3: TRzGroupBox;
    TankCB: TComboBox;
    PrintPanel: TPanel;
    UpdatePanel: TPanel;
    CopyPasteGroupBox: TRzGroupBox;
    CopyPasteTankLabel: TLabel;
    TabSheet18: TRzTabSheet;
    Panel18: TPanel;
    SLAParamRegNum61: TSLAParamRegNum;
    SLAParamRegNum62: TSLAParamRegNum;
    SLAParamRegTemp1: TSLAParamRegTemp;
    SLAParamRegOpt63: TSLAParamRegOpt;
    Label14: TLabel;
    lbl1: TLabel;
    lbl5: TLabel;
    SLAParamRegNum57: TSLAParamRegNum;
    SLAParamRegOpt16: TSLAParamRegOpt;
    SLAParamRegOpt17: TSLAParamRegOpt;
    SLAParamRegOpt56: TSLAParamRegOpt;
    SLAParamRegOpt18: TSLAParamRegOpt;
    SLAParamRegScrl26: TSLAParamRegScrl;
    procedure btnCloseClick( Sender: TObject );
  private
  public
  end;

var
  frmSHUK_PT_MBParams: TfrmSHUK_PT_MBParams;

implementation

{$R *.DFM}

procedure TfrmSHUK_PT_MBParams.btnCloseClick( Sender: TObject );
begin
  Close;
end;

end.

