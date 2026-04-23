unit main;


{$mode objfpc}{$H+}


interface


uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  DefaultTranslator,
  LCLTranslator,
  // ---
  //gauges,
  //spinbutton,
  paramregscrl,
  paramregopt,
  paramregnum,
  paramregbyte,
  paramregtemp,
  //
  ysparamreg,
  ysparamreg2,
  ysrealparamreg,
  ysbitparamreg,
  yssmallparamreg,
  //
  sypanel,
  sybutton
  ;


type

  { TMainForm }

  TMainForm = class(TForm)
    Edit1: TEdit;
    Ru: TButton;
    En: TButton;
    CloseButton: TSYButton;
    CloseButton1: TSYButton;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    ParamRegByte1: TParamRegByte;
    ParamRegNum1: TParamRegNum;
    ParamRegOpt1: TParamRegOpt;
    ParamRegScrl10: TParamRegScrl;
    ParamRegScrl11: TParamRegScrl;
    ParamRegScrl8: TParamRegScrl;
    ParamRegScrl9: TParamRegScrl;
    ParamRegTemp1: TParamRegTemp;
    SYPanel1: TSYPanel;
    SYPanel2: TSYPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    ToggleBox1: TToggleBox;
    YSBitParamReg1: TYSBitParamReg;
    YSBitParamReg2: TYSBitParamReg;
    YSParamReg2: TYSParamReg;
    YSParamReg2_2: TYSParamReg2;
    YSParamReg2_3: TYSParamReg2;
    YSParamReg2_4: TYSParamReg2;
    YSParamReg2_5: TYSParamReg2;
    YSParamReg3: TYSParamReg;
    YSParamReg4: TYSParamReg;
    YSRealParamReg1: TYSRealParamReg;
    YSRealParamReg2: TYSRealParamReg;
    YSRealParamReg3: TYSRealParamReg;
    YSSmallParamReg1: TYSSmallParamReg;
    procedure CloseButtonClick(Sender: TObject);
    procedure EnClick(Sender: TObject);
    procedure RuClick(Sender: TObject);
  end;


var
  MainForm: TMainForm;


implementation


{$R *.lfm}


{ TMainForm }

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EnClick(Sender: TObject);
begin
  SetDefaultLang('en');
end;

procedure TMainForm.RuClick(Sender: TObject);
begin
  SetDefaultLang('ru');
end;

end.

