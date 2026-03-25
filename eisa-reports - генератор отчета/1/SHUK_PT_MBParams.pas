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
  ComCtrls, StdCtrls, SYButton, ExtCtrls, Gauges, sComDrv4, 
  YSBitParamReg, YSSmallParamReg, YSParamReg, Printers, aEISATypes,
  aEISAUnit, YSRealParamReg, RzTabs, RzStatus, RzPanel, SLAParamRegScrl,
  SLAParamRegOpt, SLAParamRegNum, SLAParamRegByte, SLAParamRegTemp;

type
  TfrmSHUK_PT_MBParams = class( TForm )
    RFlashTimer: TTimer;
    sdParams: TSaveDialog;
    odParams: TOpenDialog;
    TopPanel: TPanel;
    btnClose: TSYButton;
    pcParamWindows: TRzPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    tsRegGas: TRzTabSheet;
    tsRegRazr: TRzTabSheet;
    tsAIN1: TRzTabSheet;
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
    SLAParamRegNum21: TSLAParamRegNum;
    SLAParamRegNum22: TSLAParamRegNum;
    SLAParamRegNum23: TSLAParamRegNum;
    SLAParamRegNum24: TSLAParamRegNum;
    SLAParamRegNum25: TSLAParamRegNum;
    SLAParamRegNum26: TSLAParamRegNum;
    SLAParamRegNum27: TSLAParamRegNum;
    SLAParamRegNum28: TSLAParamRegNum;
    SLAParamRegNum29: TSLAParamRegNum;
    SLAParamRegNum30: TSLAParamRegNum;
    SLAParamRegNum31: TSLAParamRegNum;
    SLAParamRegNum32: TSLAParamRegNum;
    SLAParamRegNum33: TSLAParamRegNum;
    SLAParamRegNum34: TSLAParamRegNum;
    SLAParamRegNum35: TSLAParamRegNum;
    SLAParamRegNum36: TSLAParamRegNum;
    SLAParamRegNum37: TSLAParamRegNum;
    SLAParamRegNum38: TSLAParamRegNum;
    SLAParamRegNum39: TSLAParamRegNum;
    SLAParamRegNum40: TSLAParamRegNum;
    PrintPanel: TPanel;
    tsTimes: TRzTabSheet;
    tsOptions3: TRzTabSheet;
    Panel6: TPanel;
    Panel7: TPanel;
    Label4: TLabel;
    UpdatePanel: TPanel;
    tsSignals: TRzTabSheet;
    Panel5: TPanel;
    SLAParamRegScrl14: TSLAParamRegScrl;
    SLAParamRegScrl15: TSLAParamRegScrl;
    SLAParamRegScrl16: TSLAParamRegScrl;
    SLAParamRegScrl22: TSLAParamRegScrl;
    SLAParamRegOpt3: TSLAParamRegOpt;
    SLAParamRegOpt5: TSLAParamRegOpt;
    CopyPasteGroupBox: TRzGroupBox;
    CopyPasteTankLabel: TLabel;
    tsOptions2: TRzTabSheet;
    Panel8: TPanel;
    SLAParamRegOpt9: TSLAParamRegOpt;
    SLAParamRegOpt10: TSLAParamRegOpt;
    SLAParamRegOpt11: TSLAParamRegOpt;
    SLAParamRegOpt12: TSLAParamRegOpt;
    tsOptions1: TRzTabSheet;
    Panel9: TPanel;
    SLAParamRegOpt16: TSLAParamRegOpt;
    SLAParamRegOpt17: TSLAParamRegOpt;
    tsOptions4: TRzTabSheet;
    Panel10: TPanel;
    SLAParamRegOpt23: TSLAParamRegOpt;
    SLAParamRegOpt24: TSLAParamRegOpt;
    SLAParamRegOpt26: TSLAParamRegOpt;
    SLAParamRegOpt27: TSLAParamRegOpt;
    tsOptions5: TRzTabSheet;
    Panel11: TPanel;
    SLAParamRegOpt30: TSLAParamRegOpt;
    SLAParamRegOpt31: TSLAParamRegOpt;
    SLAParamRegOpt32: TSLAParamRegOpt;
    SLAParamRegOpt33: TSLAParamRegOpt;
    SLAParamRegOpt34: TSLAParamRegOpt;
    SLAParamRegOpt35: TSLAParamRegOpt;
    SLAParamRegOpt36: TSLAParamRegOpt;
    SLAParamRegOpt37: TSLAParamRegOpt;
    SLAParamRegOpt38: TSLAParamRegOpt;
    SLAParamRegOpt39: TSLAParamRegOpt;
    SLAParamRegOpt40: TSLAParamRegOpt;
    SLAParamRegOpt41: TSLAParamRegOpt;
    SLAParamRegScrl26: TSLAParamRegScrl;
    SLAParamRegScrl27: TSLAParamRegScrl;
    SLAParamRegScrl28: TSLAParamRegScrl;
    SLAParamRegScrl29: TSLAParamRegScrl;
    SLAParamRegScrl30: TSLAParamRegScrl;
    SLAParamRegScrl31: TSLAParamRegScrl;
    SLAParamRegScrl32: TSLAParamRegScrl;
    SLAParamRegScrl33: TSLAParamRegScrl;
    TabSheet3: TRzTabSheet;
    Panel4: TPanel;
    SLAParamRegScrl34: TSLAParamRegScrl;
    SLAParamRegScrl35: TSLAParamRegScrl;
    SLAParamRegScrl36: TSLAParamRegScrl;
    SLAParamRegScrl37: TSLAParamRegScrl;
    SLAParamRegScrl38: TSLAParamRegScrl;
    SLAParamRegScrl39: TSLAParamRegScrl;
    SLAParamRegScrl41: TSLAParamRegScrl;
    RzGroupBox5: TRzGroupBox;
    SLAParamRegScrl40: TSLAParamRegScrl;
    SLAParamRegScrl42: TSLAParamRegScrl;
    RzGroupBox8: TRzGroupBox;
    SLAParamRegScrl47: TSLAParamRegScrl;
    SLAParamRegScrl48: TSLAParamRegScrl;
    RzGroupBox6: TRzGroupBox;
    SLAParamRegScrl43: TSLAParamRegScrl;
    SLAParamRegScrl44: TSLAParamRegScrl;
    tsRegNagr: TRzTabSheet;
    Panel12: TPanel;
    SLAParamRegScrl45: TSLAParamRegScrl;
    SLAParamRegScrl46: TSLAParamRegScrl;
    SLAParamRegScrl49: TSLAParamRegScrl;
    SLAParamRegScrl50: TSLAParamRegScrl;
    SLAParamRegScrl51: TSLAParamRegScrl;
    SLAParamRegScrl52: TSLAParamRegScrl;
    SLAParamRegScrl53: TSLAParamRegScrl;
    RzGroupBox7: TRzGroupBox;
    SLAParamRegScrl54: TSLAParamRegScrl;
    SLAParamRegScrl55: TSLAParamRegScrl;
    RzGroupBox9: TRzGroupBox;
    SLAParamRegScrl56: TSLAParamRegScrl;
    SLAParamRegScrl57: TSLAParamRegScrl;
    RzGroupBox10: TRzGroupBox;
    SLAParamRegScrl58: TSLAParamRegScrl;
    SLAParamRegScrl59: TSLAParamRegScrl;
    SLAParamRegScrl11: TSLAParamRegScrl;
    SLAParamRegScrl12: TSLAParamRegScrl;
    TabSheet13: TRzTabSheet;
    tsAIN2: TRzTabSheet;
    Panel13: TPanel;
    tsAIN3: TRzTabSheet;
    Panel14: TPanel;
    SLAParamRegNum1: TSLAParamRegNum;
    SLAParamRegNum2: TSLAParamRegNum;
    SLAParamRegNum3: TSLAParamRegNum;
    SLAParamRegNum4: TSLAParamRegNum;
    SLAParamRegNum5: TSLAParamRegNum;
    SLAParamRegNum6: TSLAParamRegNum;
    SLAParamRegNum7: TSLAParamRegNum;
    SLAParamRegNum8: TSLAParamRegNum;
    SLAParamRegNum9: TSLAParamRegNum;
    SLAParamRegNum10: TSLAParamRegNum;
    SLAParamRegNum11: TSLAParamRegNum;
    SLAParamRegNum12: TSLAParamRegNum;
    SLAParamRegNum13: TSLAParamRegNum;
    SLAParamRegNum14: TSLAParamRegNum;
    SLAParamRegNum15: TSLAParamRegNum;
    SLAParamRegNum16: TSLAParamRegNum;
    SLAParamRegNum17: TSLAParamRegNum;
    SLAParamRegNum18: TSLAParamRegNum;
    SLAParamRegNum19: TSLAParamRegNum;
    SLAParamRegNum20: TSLAParamRegNum;
    SLAParamRegOpt42: TSLAParamRegOpt;
    SLAParamRegOpt43: TSLAParamRegOpt;
    SLAParamRegOpt44: TSLAParamRegOpt;
    SLAParamRegOpt45: TSLAParamRegOpt;
    SLAParamRegOpt46: TSLAParamRegOpt;
    SLAParamRegOpt47: TSLAParamRegOpt;
    SLAParamRegOpt48: TSLAParamRegOpt;
    SLAParamRegOpt49: TSLAParamRegOpt;
    SLAParamRegOpt50: TSLAParamRegOpt;
    SLAParamRegOpt51: TSLAParamRegOpt;
    SLAParamRegNum41: TSLAParamRegNum;
    SLAParamRegNum42: TSLAParamRegNum;
    SLAParamRegNum43: TSLAParamRegNum;
    SLAParamRegNum44: TSLAParamRegNum;
    SLAParamRegNum45: TSLAParamRegNum;
    SLAParamRegNum46: TSLAParamRegNum;
    SLAParamRegNum47: TSLAParamRegNum;
    SLAParamRegNum48: TSLAParamRegNum;
    SLAParamRegOpt52: TSLAParamRegOpt;
    SLAParamRegOpt53: TSLAParamRegOpt;
    SLAParamRegOpt54: TSLAParamRegOpt;
    SLAParamRegOpt55: TSLAParamRegOpt;
    SLAParamRegOpt56: TSLAParamRegOpt;
    SLAParamRegOpt18: TSLAParamRegOpt;
    SLAParamRegOpt57: TSLAParamRegOpt;
    SLAParamRegOpt19: TSLAParamRegOpt;
    SLAParamRegOpt20: TSLAParamRegOpt;
    SLAParamRegOpt21: TSLAParamRegOpt;
    SLAParamRegOpt1: TSLAParamRegOpt;
    TabSheet16: TRzTabSheet;
    Panel15: TPanel;
    RzGroupBox4: TRzGroupBox;
    SLAParamRegByte1: TSLAParamRegByte;
    SLAParamRegByte2: TSLAParamRegByte;
    SLAParamRegByte3: TSLAParamRegByte;
    RzGroupBox11: TRzGroupBox;
    SLAParamRegByte4: TSLAParamRegByte;
    SLAParamRegByte5: TSLAParamRegByte;
    SLAParamRegByte6: TSLAParamRegByte;
    SLAParamRegNum49: TSLAParamRegNum;
    SLAParamRegNum50: TSLAParamRegNum;
    SLAParamRegOpt15: TSLAParamRegOpt;
    SLAParamRegNum51: TSLAParamRegNum;
    SLAParamRegNum52: TSLAParamRegNum;
    SLAParamRegOpt22: TSLAParamRegOpt;
    SLAParamRegNum53: TSLAParamRegNum;
    SLAParamRegNum54: TSLAParamRegNum;
    SLAParamRegOpt25: TSLAParamRegOpt;
    SLAParamRegNum55: TSLAParamRegNum;
    SLAParamRegNum56: TSLAParamRegNum;
    SLAParamRegOpt28: TSLAParamRegOpt;
    SLAParamRegScrl13: TSLAParamRegScrl;
    SLAParamRegScrl17: TSLAParamRegScrl;
    SLAParamRegScrl18: TSLAParamRegScrl;
    SLAParamRegScrl19: TSLAParamRegScrl;
    SLAParamRegScrl20: TSLAParamRegScrl;
    SLAParamRegScrl21: TSLAParamRegScrl;
    SLAParamRegOpt59: TSLAParamRegOpt;
    SLAParamRegOpt60: TSLAParamRegOpt;
    Panel16: TPanel;
    Label7: TLabel;
    tsOptions6: TRzTabSheet;
    Panel17: TPanel;
    SLAParamRegOpt2: TSLAParamRegOpt;
    SLAParamRegOpt6: TSLAParamRegOpt;
    SLAParamRegOpt7: TSLAParamRegOpt;
    SLAParamRegOpt8: TSLAParamRegOpt;
    SLAParamRegOpt61: TSLAParamRegOpt;
    SLAParamRegOpt62: TSLAParamRegOpt;
    Label12: TLabel;
    Label13: TLabel;
    RzGroupBox12: TRzGroupBox;
    SLAParamRegScrl7: TSLAParamRegScrl;
    SLAParamRegScrl8: TSLAParamRegScrl;
    SLAParamRegScrl9: TSLAParamRegScrl;
    SLAParamRegScrl10: TSLAParamRegScrl;
    SLAParamRegScrl60: TSLAParamRegScrl;
    SLAParamRegScrl61: TSLAParamRegScrl;
    SLAParamRegScrl62: TSLAParamRegScrl;
    SLAParamRegScrl63: TSLAParamRegScrl;
    SLAParamRegScrl64: TSLAParamRegScrl;
    tsAST: TRzTabSheet;
    Panel18: TPanel;
    SLAParamRegNum61: TSLAParamRegNum;
    SLAParamRegNum62: TSLAParamRegNum;
    SLAParamRegTemp1: TSLAParamRegTemp;
    SLAParamRegOpt63: TSLAParamRegOpt;
    Label14: TLabel;
    SLAParamRegNum63: TSLAParamRegNum;
    SLAParamRegNum64: TSLAParamRegNum;
    SLAParamRegOpt64: TSLAParamRegOpt;
    SLAParamRegTemp2: TSLAParamRegTemp;
    SLAParamRegNum65: TSLAParamRegNum;
    SLAParamRegNum66: TSLAParamRegNum;
    SLAParamRegTemp3: TSLAParamRegTemp;
    SLAParamRegOpt65: TSLAParamRegOpt;
    SLAParamRegNum67: TSLAParamRegNum;
    SLAParamRegNum68: TSLAParamRegNum;
    SLAParamRegOpt66: TSLAParamRegOpt;
    SLAParamRegTemp4: TSLAParamRegTemp;
    SLAParamRegNum69: TSLAParamRegNum;
    SLAParamRegNum70: TSLAParamRegNum;
    SLAParamRegTemp5: TSLAParamRegTemp;
    SLAParamRegOpt67: TSLAParamRegOpt;
    SLAParamRegNum71: TSLAParamRegNum;
    SLAParamRegNum72: TSLAParamRegNum;
    SLAParamRegOpt68: TSLAParamRegOpt;
    SLAParamRegTemp6: TSLAParamRegTemp;
    Label1: TLabel;
    Label2: TLabel;
    BtnAutoReg1: TSYButton;
    Label16: TLabel;
    Label31: TLabel;
    Label33: TLabel;
    Label35: TLabel;
    Label17: TLabel;
    SLAParamRegNum57: TSLAParamRegNum;
    SLAParamRegNum58: TSLAParamRegNum;
    SLAParamRegNum59: TSLAParamRegNum;
    SLAParamRegNum60: TSLAParamRegNum;
    Label18: TLabel;
    Label19: TLabel;
    SLAParamRegNum73: TSLAParamRegNum;
    Label20: TLabel;
    SLAParamRegNum74: TSLAParamRegNum;
    Label21: TLabel;
    SLAParamRegNum75: TSLAParamRegNum;
    Label22: TLabel;
    SLAParamRegNum76: TSLAParamRegNum;
    Label23: TLabel;
    Label24: TLabel;
    SLAParamRegNum77: TSLAParamRegNum;
    Label25: TLabel;
    SLAParamRegNum78: TSLAParamRegNum;
    Label26: TLabel;
    SLAParamRegNum79: TSLAParamRegNum;
    Label27: TLabel;
    SLAParamRegNum80: TSLAParamRegNum;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label32: TLabel;
    Label34: TLabel;
    SLAParamRegNum81: TSLAParamRegNum;
    SLAParamRegNum82: TSLAParamRegNum;
    SLAParamRegNum83: TSLAParamRegNum;
    SLAParamRegNum84: TSLAParamRegNum;
    tsRegMazut: TRzTabSheet;
    SLAParamRegScrl65: TSLAParamRegScrl;
    SLAParamRegScrl66: TSLAParamRegScrl;
    SLAParamRegScrl67: TSLAParamRegScrl;
    SLAParamRegScrl68: TSLAParamRegScrl;
    SLAParamRegScrl69: TSLAParamRegScrl;
    SLAParamRegScrl70: TSLAParamRegScrl;
    SLAParamRegScrl71: TSLAParamRegScrl;
    Panel19: TPanel;
    SLAParamRegScrl72: TSLAParamRegScrl;
    SLAParamRegOpt13: TSLAParamRegOpt;
    SLAParamRegOpt14: TSLAParamRegOpt;
    SLAParamRegOpt29: TSLAParamRegOpt;
    SLAParamRegOpt58: TSLAParamRegOpt;
    SLAParamRegOpt69: TSLAParamRegOpt;
    SLAParamRegOpt70: TSLAParamRegOpt;
    SLAParamRegOpt4: TSLAParamRegOpt;
    Label9: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label15: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    SLAParamRegScrl1: TSLAParamRegScrl;
    SLAParamRegScrl2: TSLAParamRegScrl;
    SLAParamRegScrl3: TSLAParamRegScrl;
    SLAParamRegScrl4: TSLAParamRegScrl;
    SLAParamRegScrl5: TSLAParamRegScrl;
    SLAParamRegScrl6: TSLAParamRegScrl;
    SLAParamRegScrl23: TSLAParamRegScrl;
    SLAParamRegScrl24: TSLAParamRegScrl;
    SLAParamRegScrl25: TSLAParamRegScrl;
    Y0SLAParamReg: TSLAParamRegScrl;
    Y1SLAParamReg: TSLAParamRegScrl;
    Y2SLAParamReg: TSLAParamRegScrl;
    Y3SLAParamReg: TSLAParamRegScrl;
    Y4SLAParamReg: TSLAParamRegScrl;
    Y1_1SLAParamReg: TSLAParamRegScrl;
    Y1_2SLAParamReg: TSLAParamRegScrl;
    Y1_3SLAParamReg: TSLAParamRegScrl;
    pnlGrPanel1: TPanel;
    ZeroLabel: TLabel;
    X1Label: TLabel;
    lbl3: TLabel;
    Y3Label: TLabel;
    Y2Label: TLabel;
    Y1Label: TLabel;
    Y0Label: TLabel;
    lbl5: TLabel;
    Y4Label: TLabel;
    X1_2Label: TLabel;
    X1_3Label: TLabel;
    X2Label: TLabel;
    X3Label: TLabel;
    X4Label: TLabel;
    Y1_1Label: TLabel;
    Y1_2Label: TLabel;
    Y1_3Label: TLabel;
    lbl6: TLabel;
    lbl7: TLabel;
    lbl8: TLabel;
    lbl1: TLabel;
    X2_1Label: TLabel;
    lbl9: TLabel;
    lbl10: TLabel;
    X3_1Label: TLabel;
    lbl12: TLabel;
    PBPanel1: TPanel;
    PaintBox1: TPaintBox;
    X1_1Label: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Y2_1SLAParamReg: TSLAParamRegScrl;
    Label60: TLabel;
    Label61: TLabel;
    Y3_1SLAParamReg: TSLAParamRegScrl;
    Label62: TLabel;
    Label63: TLabel;
    Y2_1Label: TLabel;
    Y3_1Label: TLabel;
    procedure PrintButtonClick( Sender: TObject );
    procedure CopyBuffButtonClick( Sender: TObject );
    procedure PasteBuffButtonClick( Sender: TObject );
    procedure RFlashTimerTimer( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure FormClose( Sender: TObject; var Action: TCloseAction );
    procedure FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure NextWindowButtonClick( Sender: TObject );
    procedure btnSaveParamClick( Sender: TObject );
    procedure TankCBChange( Sender: TObject );
    procedure btnWriteClick( Sender: TObject );
    procedure btnCloseClick( Sender: TObject );
    procedure btnLoadParamClick( Sender: TObject );
    procedure TankCBCloseUp(Sender: TObject);
    procedure Y0SLAParamRegChange(Sender: TObject);
    procedure BtnAutoReg1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FUpdateParamsOneShot: SmallInt;
    FPanelEEPROMBlinker: SmallInt;
    procedure SetUpdateParamsOneShot(const Value: SmallInt);
    procedure SetPanelEEPROMBlinker(const Value: SmallInt);
  private
    { Private declarations }
    LinkArray: array of Byte;
    CurrentTankABN:  Byte;          //Текущий котел (номер абонента в массиве ABN)
    cABN, cBUF:      Integer;       //В FormShow конвертируем в них CurrentLogicABN и CurrentLogicBUF
    OldLinkState:    ShortInt;      //Предыдущее состояние связи с абонентом (буфером)
    CopyPasteMass:   TFlashBuffer;  //Буфер для Copy/Paste
    GraphMaxPg1 :    Single;        //Максимумы давления газа для графика
    //Счетчик обновления данных на экране (в циклах таймера); если = 0 то выключено
    property UpdateParamsOneShot: SmallInt read FUpdateParamsOneShot write SetUpdateParamsOneShot;
    //Счетчик мигания кнопкой "Запись" (в циклах таймера); если = 0 то выключено
    property PanelEEPROMBlinker: SmallInt read FPanelEEPROMBlinker write SetPanelEEPROMBlinker;
    procedure FillTankCB;
    procedure CalculateSLAParamRegsGraph;
  public
    { Public declarations }
  end;

var
  frmSHUK_PT_MBParams: TfrmSHUK_PT_MBParams;

implementation

uses
  aMain, aEISATools, sCOMdrv1, SLAParamRegFunc, sMethods, DigitalInput,
  AutoRegulator;

{$R *.DFM}

const
  MaxPgPercent = 0.4; //SLA вертикальная шкала max 40% от датчика P газа

procedure TfrmSHUK_PT_MBParams.FillTankCB;
var
  i, j, k: SmallInt;
begin
  //Запоняем TComboBox TankCB с названиями котлов
  TankCB.Clear;
  k := 0;
  for i := 1 to AbnCount do
    if ABN[i].BUF[0].ModuleType = SHUK_PT_MB then
      begin
        TankCB.Items.Add( ABN[i].Name );
        Inc( k );
      end;
  SetLength( LinkArray, k );
  k := 0;
  for i := 1 to AbnCount do
    if ABN[i].BUF[0].ModuleType = SHUK_PT_MB then
      begin
        LinkArray[k] := i;
        Inc( k );
      end;
  TankCB.ItemIndex := 0;
  CurrentTankABN := LinkArray[0];
  for j := 0 to Length( LinkArray ) - 1 do
    begin
      if LinkArray[j] = CurrentABN then
        begin
          TankCB.ItemIndex := j;
          CurrentTankABN := CurrentABN;
          break;
        end;
    end;
end;

procedure TfrmSHUK_PT_MBParams.PrintButtonClick( Sender: TObject );
begin
  //Печать текущей странички
  PrintPanel.Top := 5;
  PrintParamsForm( Self, PrintPanel );
  PrintPanel.Top := -100;
end;

procedure TfrmSHUK_PT_MBParams.FormShow( Sender: TObject );
begin
  //Shekhter проверить
  ConvertABN(CurrentLogicABN, CurrentLogicBUF, cABN, cBUF);
  GraphMaxPg1 := 0;

  FillTankCB;
  TankCBChange(Sender);
  //Сброс Copy/Paste
  CopyPasteGroupBox.Visible := False;
  PasteBuffButton.Enabled := False;
  //Включение таймера
  RFlashTimer.Enabled := True;
end;

procedure TfrmSHUK_PT_MBParams.FormClose( Sender: TObject; var Action: TCloseAction );
begin
  //Очистка буфера записи
  Flash_ClearWrite(CurrentTankABN, 0);
  //Отключение таймера
  RFlashTimer.Enabled := False;
  LinkArray := nil;
end;

procedure TfrmSHUK_PT_MBParams.RFlashTimerTimer( Sender: TObject );
var
  Progr:  Integer;
  Total:  Integer;
begin
  RFlashTimer.Enabled := False;
  if CurrentTankABN in [1..cMaxABN] then
  begin
    //Проверка состояния связи
    if IsLinkBUF(CurrentTankABN, 0) = clsLink then
      begin
        if (OldLinkState = clsNoLink) and (FLASH_ReadComplete(CurrentTankABN, 0)) then
          TankCBChange(Sender);
        //------- Процессы чтения и записи EEPROM -------
        //Ползунок цикличного чтения
        FLASH_ReadPos( CurrentTankABN, 0, Progr, Total );
        Progr := Progr + cMBEEPROMIncrement * 2; //SLA - указываем на хвост, а не на голову
        RGauge.Percent := Round(Progr / Total * 100);
        //Проценты общего ЧТЕНИЯ EEPROM
        RPercent.Percent := Round(FLASH_ReadPercent(CurrentTankABN, 0));
        if FLASH_ReadComplete(CurrentTankABN, 0) then
          begin //Бледный цвет, чтение закончено
            RPercent.BarColor := $00B5BEC8;
            RPercent.BarColorStop := $00D0D7DD;
            LabelRd.Font.Color := $00A3AAAF;
          end
        else
          begin //Яркий цвет, идёт чтение
            RPercent.BarColor := $00FF8000;
            RPercent.BarColorStop := $009B4E00;
            LabelRd.Font.Color := clBlack;
          end;
        //Проценты общей ЗАПИСИ EEPROM
        WPercent.Percent := Trunc( FLASH_WritePercent( CurrentTankABN, 0 ) );
        if Flash_Writing(CurrentTankABN, 0) then
          begin //Яркий цвет, идёт запись
            WPercent.BarColor := clRed;
            WPercent.BarColorStop := $0000009B;
            LabelWr.Font.Color := clBlack;
            WPercent.ShowPercent := True;
          end
        else
          begin //Бледный цвет, запись закончена
            WPercent.BarColor := $00B5BEC8;
            WPercent.BarColorStop := $00D0D7DD;
            LabelWr.Font.Color := $00A3AAAF;
            WPercent.ShowPercent := False;
          end;

        //------- Основная работа, если чтение EEPROM закончено -------
        if FLASH_ReadComplete(CurrentTankABN, 0) then
          with ABN[CurrentTankABN].BUF[0] do
          begin
            //Проверка состояния записи в контроллере
            if Flash_Writing_OnController(CurrentTankABN, 0) then
              UpdateParamsOneShot := constUpdateParamsCycles;
            //Калибровка, расчет и обновление параметров на форме
            if Assigned(EEPROMcfg) and (UpdateParamsOneShot > 0) then
              begin
                UpdateParamsOneShot := UpdateParamsOneShot - 1;
                //Расчет параметров в коллекции
                EEPROMcfg.CalculateParams;
                //Калибровка параметров текущей формы
                CalibrateParamRegsOfForm(Self, EEPROMcfg);
                //Чтение значений параметров текущей формы
                ReadParamRegsOfForm(Self, EEPROMcfg);
                //Загружаем максимумы для графика №1 соотношения "ГАЗ-ВОЗДУХ" и рисуем
                GraphMaxPg1 := Y0SLAParamReg.Calibrate.Max;
                CalculateSLAParamRegsGraph;
              end;
          end;
      end
    else // Если связи нет, сбрасываем значения
      ClearParamsReg( Self, nil, nil );
  end;
  //Уменьшаем счетчик блинкера
  if PanelEEPROMBlinker > 0 then
    PanelEEPROMBlinker := PanelEEPROMBlinker - 1;
  //Связываем кнопку "Записать" с активностью привилегированного режима
  if (not btnWrite.Enabled) and MainForm.PrivilegeMode then
    btnWrite.Enabled := True;
  if btnWrite.Enabled and (not MainForm.PrivilegeMode) then
    btnWrite.Enabled := False;
  //Завершение таймера
  RFlashTimer.Enabled := True;
end;

procedure TfrmSHUK_PT_MBParams.FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
begin
  if Shift = [] then
    begin
      case Key of
        KWriteKoef:      if btnWrite.Enabled and
                            (Application.MessageBox( 'Выполнить запись измененных параметров в ШУK-ПТ100 ?', PChar( csMBConfirmationCaption ), mb_YesNo + MB_IconQuestion ) = idYes) then
                            begin
                              WriteParamRegsOfForm(Self, CurrentTankABN, 0);
                              PanelEEPROMBlinker := 10; //Помигаем кнопкой, джентльмены
                            end;
        KCopyBuff:       CopyBuffButtonClick( nil );
        KPasteBuff:      PasteBuffButtonClick( nil );
        KBoilerParams:   Close;
      end;
    end;
end;

procedure TfrmSHUK_PT_MBParams.NextWindowButtonClick( Sender: TObject );
begin
  pcParamWindows.SelectNextPage( True );
end;

procedure TfrmSHUK_PT_MBParams.TankCBChange(Sender: TObject);
begin
  CurrentTankABN := LinkArray[TankCB.ItemIndex];
  pcParamWindows.SetFocus;
  if CurrentTankABN in [1..cMaxABN] then
    begin
      //Подготовка компонентов
      PrepareAllParamRegsOfForm(Self, ABN[CurrentTankABN].BUF[0].EEPROMcfg);
      //Проверка связи
      OldLinkState := IsLinkBUF(CurrentTankABN, 0);
      if OldLinkState = clsNoLink then
        begin
          Application.MessageBox( 'Нет связи с ШУК-ПТ100'#10'Невозможно произвести чтение параметров', PChar( csMBWarningCaption ), MB_OK + MB_IconExclamation );
          UpdateParamsOneShot := 0;
        end
      else
        UpdateParamsOneShot := constUpdateParamsCycles;
      //Выравниваем значения в компонентах SLAParamReg
      SetValuesEqual(Self);  
    end;
end;

procedure TfrmSHUK_PT_MBParams.TankCBCloseUp(Sender: TObject);
begin
  pcParamWindows.SetFocus;
end;

procedure TfrmSHUK_PT_MBParams.btnWriteClick( Sender: TObject );
begin
  with DigitalInputForm do
    begin
      PasswordMode := True;
      Position := poScreenCenter;
      if ShowModal = mrOk then
        if Round(ResultReal) = SaveParamRegPasswordNumber then
          begin
            WriteParamRegsOfForm(Self, CurrentTankABN, 0); //Запись разрешаем
            PanelEEPROMBlinker := 10; //Помигаем кнопкой, джентльмены
          end
        else
          Application.MessageBox('Пароль введен неверно, запись параметров не выполнена!', 'Ошибка', mb_Ok + mb_IconError);
    end;
end;

procedure TfrmSHUK_PT_MBParams.btnCloseClick( Sender: TObject );
begin
  Close;
end;

procedure TfrmSHUK_PT_MBParams.CopyBuffButtonClick( Sender: TObject );
var
  MaxAdr: Integer;
begin
  // Скопировать в буфер
  MaxAdr := Flash_GetMaxAdr( CurrentTankABN, 0 );
  if ( MaxAdr > 1 ) and FLASH_ReadComplete( CurrentTankABN, 0 ) then
    begin
      CopyPasteMass := Flash[CurrentTankABN, 0].Reci;
      CopyPasteTankLabel.Caption := ABN[CurrentTankABN].Name;
      CopyPasteGroupBox.Visible := True;
      PasteBuffButton.Enabled := True;
      // Подаем звук
      MainForm.PlaySound( cSoundMenu );
    end;
end;

procedure TfrmSHUK_PT_MBParams.PasteBuffButtonClick( Sender: TObject );
begin
  // Извлечь из буфера
  // Загрузка из CopyPasteMass в компоненты.EditValue
  LoadEditValuesFromTempMas( Self, @CopyPasteMass );
  // Подаем звук
  MainForm.PlaySound( cSoundMenu );
end;

procedure TfrmSHUK_PT_MBParams.btnSaveParamClick( Sender: TObject );
var
  i, MaxAdr: Integer;
  F:         file of TOneParam;
  D:         TOneParam;
  Name:      String;
begin
  //Сохранить в файл
  MaxAdr := Flash_GetMaxAdr(CurrentTankABN, 0);
  if (MaxAdr > 1) {and FLASH_ReadComplete(CurrentTankABN, 0)} then
    try
      //Типизированный файл
      Name := StartingDir + LogDir + 'EEPROM ' + ABN[CurrentTankABN].Name + ABN[CurrentTankABN].BUF[0].Name + ' (' + FormatDateTime( 'dd.mm.yyyy hh.nn.ss', Now ) + ').eisafp';
      AssignFile( F, Name );
      Rewrite( F );
      //Сохраняем с 0 байта по максимальный
      for i := 0 to MaxAdr do
        begin
          D.Place := i;
          D.Data := Flash_Get( CurrentTankABN, 0, i, 8 );
          Write( F, D );
        end;
    finally
      CloseFile( F );
      Application.MessageBox(PChar('Параметры данного окна сохранены в файл:'#10 + Name), PChar(csMBInformationCaption), MB_OK + MB_IconInformation);
    end;
end;

procedure TfrmSHUK_PT_MBParams.btnLoadParamClick( Sender: TObject );
var
  TempMass:  TFlashBuffer;  //Временный буфер для различных операций
  i, MaxAdr: Integer;
  F:         file of TOneParam;
  D:         TOneParam;
  Name:      String;
begin
  //Загрузить из файла ...
  MaxAdr := Flash_GetMaxAdr(CurrentTankABN, 0);
  if (MaxAdr > 1) {and FLASH_ReadComplete(CurrentTankABN, 0)} then
    begin
      odParams.InitialDir := StartingDir + LogDir;
      if odParams.Execute then
        begin
          Name := odParams.FileName;
          try
            //Чтение из файла в массив TempMass
            AssignFile( F, Name );
            Reset( F );
            i := 0;
            while ( (not eof(F)) or (i <= MaxAdr) ) do
              begin
                Read(F, D);
                TempMass[D.Place] := D.Data;
                Inc(i);
              end;
            //Загрузка из TempMass в компоненты.EditValue
            LoadEditValuesFromTempMas(Self, @TempMass);

          finally
            CloseFile( F );
          end;
        end;
    end;
end;

procedure TfrmSHUK_PT_MBParams.SetUpdateParamsOneShot(const Value: SmallInt);
begin
  FUpdateParamsOneShot := Value;
  if Value > 0 then
    UpdatePanel.Color := clLime
  else
    UpdatePanel.Color := $00D7DADF;
end;

procedure TfrmSHUK_PT_MBParams.CalculateSLAParamRegsGraph;
var
  StepY: Real;
  i: Integer;
begin
  //Определяем шаг по осям
  if GraphMaxPg1 > 0 then
    StepY := (PBPanel1.Height / GraphMaxPg1) / MaxPgPercent //SLA Нефтекамск 15.12.2017
  else
    StepY := 0;
  //Вычисляем координаты меток Y
  i := PBPanel1.Top + PBPanel1.Height - 1;
  Y0Label.Top := Round( i - Y0SLAParamReg.EditValue * StepY ) - Yofs; if Y0Label.Top < PBPanel1.Top - Yofs then Y0Label.Top := PBPanel1.Top - Yofs;
  Y1Label.Top := Round( i - Y1SLAParamReg.EditValue * StepY ) - Yofs; if Y1Label.Top < PBPanel1.Top - Yofs then Y1Label.Top := PBPanel1.Top - Yofs;
  Y1_1Label.Top := Round( i - Y1_1SLAParamReg.EditValue * StepY ) - Yofs; if Y1_1Label.Top < PBPanel1.Top - Yofs then Y1_1Label.Top := PBPanel1.Top - Yofs;
  Y1_2Label.Top := Round( i - Y1_2SLAParamReg.EditValue * StepY ) - Yofs; if Y1_2Label.Top < PBPanel1.Top - Yofs then Y1_2Label.Top := PBPanel1.Top - Yofs;
  Y1_3Label.Top := Round( i - Y1_3SLAParamReg.EditValue * StepY ) - Yofs; if Y1_3Label.Top < PBPanel1.Top - Yofs then Y1_3Label.Top := PBPanel1.Top - Yofs;
  Y2Label.Top := Round( i - Y2SLAParamReg.EditValue * StepY ) - Yofs; if Y2Label.Top < PBPanel1.Top - Yofs then Y2Label.Top := PBPanel1.Top - Yofs;
  Y2_1Label.Top := Round( i - Y2_1SLAParamReg.EditValue * StepY ) - Yofs; if Y2_1Label.Top < PBPanel1.Top - Yofs then Y2_1Label.Top := PBPanel1.Top - Yofs;
  Y3Label.Top := Round( i - Y3SLAParamReg.EditValue * StepY ) - Yofs; if Y3Label.Top < PBPanel1.Top - Yofs then Y3Label.Top := PBPanel1.Top - Yofs;
  Y3_1Label.Top := Round( i - Y3_1SLAParamReg.EditValue * StepY ) - Yofs; if Y3_1Label.Top < PBPanel1.Top - Yofs then Y3_1Label.Top := PBPanel1.Top - Yofs;
  Y4Label.Top := Round( i - Y4SLAParamReg.EditValue * StepY ) - Yofs; if Y4Label.Top < PBPanel1.Top - Yofs then Y4Label.Top := PBPanel1.Top - Yofs;
  PaintBox1.Repaint;
end;

procedure TfrmSHUK_PT_MBParams.SetPanelEEPROMBlinker(const Value: SmallInt);
begin
  FPanelEEPROMBlinker := Value;
  {if Value > 0 then
    EEPROMRzGroupBox.Transparent := not EEPROMRzGroupBox.Transparent
  else
    EEPROMRzGroupBox.Transparent := True; }
  if Value > 0 then
    btnWrite.Visible := not btnWrite.Visible
  else
    btnWrite.Visible := True;
end;

procedure TfrmSHUK_PT_MBParams.Y0SLAParamRegChange(Sender: TObject);
begin
  CalculateSLAParamRegsGraph;
end;

procedure TfrmSHUK_PT_MBParams.BtnAutoReg1Click(Sender: TObject);
begin
  //Автонастройка регулятора газа
  with AutoRegulatorForm do
    begin
      aRegIndex := 1;
      mABN := cABN;
      mBUF := cBUF;
      ShowModal;
    end;
end;

procedure TfrmSHUK_PT_MBParams.PaintBox1Paint(Sender: TObject);
begin
  PaintGraphX6Y10PTVM( PBPanel1, PaintBox1, $002B5500, $0080FF00,
             X1Label, X1_1Label, X1_2Label, X1_3Label, X2Label, X2_1Label, X3Label, X3_1Label,
    Y0Label, Y1Label, Y1_1Label, Y1_2Label, Y1_3Label, Y2Label, Y2_1Label, Y3Label, Y3_1Label, Y4Label );
end;

initialization
  RegisterClass( TfrmSHUK_PT_MBParams );

end.

