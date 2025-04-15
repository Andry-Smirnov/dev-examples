unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  sybutton
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    SYButton1: TSYButton;
    SYButton2: TSYButton;
    SYButton3: TSYButton;
    SYButton4: TSYButton;
    SYButton5: TSYButton;
    SYButton6: TSYButton;
    SYButton7: TSYButton;
    SYButton8: TSYButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

