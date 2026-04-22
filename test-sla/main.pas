unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  paramregscrl, paramregopt, paramregnum, paramregbyte,
  paramregtemp, gauges, spinbutton, ysrealparamreg, ysparamreg2, ysparamreg,
  ysbitparamreg, yssmallparamreg, sypanel, sybutton;

type

  { TMainForm }

  TMainForm = class(TForm)
    CloseButton: TSYButton;
    CloseButton1: TSYButton;
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
    YSBitParamReg1: TYSBitParamReg;
    YSBitParamReg2: TYSBitParamReg;
    YSParamReg2: TYSParamReg;
    YSParamReg2_1: TYSParamReg2;
    YSParamReg2_2: TYSParamReg2;
    YSParamReg3: TYSParamReg;
    YSRealParamReg1: TYSRealParamReg;
    YSRealParamReg2: TYSRealParamReg;
    YSRealParamReg3: TYSRealParamReg;
    YSSmallParamReg1: TYSSmallParamReg;
    procedure CloseButtonClick(Sender: TObject);
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

end.

