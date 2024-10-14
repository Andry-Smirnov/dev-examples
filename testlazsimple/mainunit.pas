unit mainunit;


{$mode objfpc}{$H+}


interface


uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  uPSComponent,
  JvNavigationPane,
  ColorSpeedButton,
  BCButton,
  BCButtonFocus,
  EpikTimer,
  UniqueInstance,
  rxctrls,
  uEButton,
  //
  ParamRegByte,
  ParamRegTemp,
  yssmallparamreg,
  ysparamreg,
  sybutton;


type

  { TMainForm }

  TMainForm = class(TForm)
    BCButton1: TBCButton;
    BCButtonFocus1: TBCButtonFocus;
    ColorSpeedButton1: TColorSpeedButton;
    EpikTimer1: TEpikTimer;
    JvNavPanelButton1: TJvNavPanelButton;
    ParamRegByte1: TParamRegByte;
    ParamRegByte2: TParamRegByte;
    ExitButton: TSYButton;
    ParamRegTemp1: TParamRegTemp;
    PSScript1: TPSScript;
    RxSpeedButton1: TRxSpeedButton;
    uEButton1: TuEButton;
    UniqueInstance1: TUniqueInstance;
    YSParamReg1: TYSParamReg;
    YSSmallParamReg1: TYSSmallParamReg;

    procedure ExitButtonClick(Sender: TObject);
    procedure NiceChart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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

procedure TMainForm.NiceChart1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;


end.

