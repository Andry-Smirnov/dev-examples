unit mainunit;


{$mode objfpc}{$H+}
{$codepage utf8}


interface


uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, vsVisualSynapse,
  TplButtonUnit, TplButtonExUnit, cyButton, cyCheckbox, Codebot.Controls.Extras,
  uPSComponent, CheckBoxThemed, ExtendedTabControls, LazNumEdit, SpinEx,
  JvNavigationPane, ColorSpeedButton, BCButton, BCButtonFocus,
  //EpikTimer,
  UniqueInstance,
  rxctrls,
  uEButton,
  //uEMultiTurn,
  //
  ParamRegByte, ParamRegTemp, yssmallparamreg, ysparamreg, sybutton, emisgrid,
  DataPortIP, FXMaterialButton, FluentButtons, ECGrid, attabs,
  //ATGroups,
  ATButtons,
  //ATGauge,
  digitalinput,
  //Grids,
  StdCtrls;


type

  { TMainForm }

  TMainForm = class(TForm)
    ATTabs1: TATTabs;
    BCButton1: TBCButton;
    BCButtonFocus1: TBCButtonFocus;
    CheckBoxThemed1: TCheckBoxThemed;
    ColorSpeedButton1: TColorSpeedButton;
    cyButton1: TcyButton;
    cyCheckBox1: TcyCheckBox;
    DataPortTCP1: TDataPortTCP;
    ECGrid1: TECGrid;
    Edit1: TEdit;
    EmiStringGrid1: TEmiStringGrid;
    EmiStringGrid2: TEmiStringGrid;
    EmiStringGrid3: TEmiStringGrid;
    EmiStringGrid4: TEmiStringGrid;
    EmiStringGrid5: TEmiStringGrid;
    EmiStringGrid6: TEmiStringGrid;
    ExtendedTabControl1: TExtendedTabControl;
    ExtendedTabToolbar1: TExtendedTabToolbar;
    FloatSpinEditEx1: TFloatSpinEditEx;
    FluentButton1: TFluentButton;
    FXMaterialButton1: TFXMaterialButton;
    IndeterminateProgress1: TIndeterminateProgress;
    JvNavPanelButton1: TJvNavPanelButton;
    LazIntegerEdit1: TLazIntegerEdit;
    ParamRegByte1: TParamRegByte;
    ParamRegByte2: TParamRegByte;
    ExitButton: TSYButton;
    ParamRegTemp1: TParamRegTemp;
    plButton1: TplButton;
    plButtonEx1: TplButtonEx;
    PSScript1: TPSScript;
    RxSpeedButton1: TRxSpeedButton;
    uEButton1: TuEButton;
    UniqueInstance1: TUniqueInstance;
    vsVisualTCP1: TvsVisualTCP;
    YSParamReg1: TYSParamReg;
    YSSmallParamReg1: TYSSmallParamReg;

    procedure EmiStringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure EmiStringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure ExitButtonClick(Sender: TObject);
    procedure SYButton1Click(Sender: TObject);
  end;


var
  MainForm: TMainForm;


implementation


{$R *.lfm}


{ TMainForm }


procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EmiStringGrid1GetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  if Value = '' then
    Value := '123456';
end;

procedure TMainForm.EmiStringGrid1SetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  Int: Integer;
begin
  try
    Int := StrToInt(Value);
  except
  end;
  Edit1.Text := IntToStr(Int);
end;

procedure TMainForm.SYButton1Click(Sender: TObject);
begin
  DigitalInputForm.ShowModal;
end;


end.

