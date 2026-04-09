unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  paramregscrl, paramregopt, paramregnum, paramregbyte, paramregtemp, gauges,
  spinbutton, ysrealparamreg;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    ParamRegScrl1: TParamRegScrl;
    ParamRegScrl2: TParamRegScrl;
    ParamRegScrl3: TParamRegScrl;
    ParamRegScrl4: TParamRegScrl;
    ParamRegScrl5: TParamRegScrl;
    SpinButton2: TSpinButton;
    TabSheet1: TTabSheet;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    YSRealParamReg1: TYSRealParamReg;
    YSRealParamReg2: TYSRealParamReg;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

end.

