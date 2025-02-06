unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  PINGsend,
  vsping;

type

  { TForm1 }

  TForm1 = class(TForm)
    Ping2Button: TButton;
    ExitButton: TButton;
    Memo1: TMemo;
    PingButton: TButton;
    IPEdit: TEdit;
    IPLabel: TLabel;
    vsSynPing1: TvsSynPing;
    PingSend: TPINGSend;
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Ping2ButtonClick(Sender: TObject);
    procedure PingButtonClick(Sender: TObject);
    procedure vsSynPing1Status(Sender: TObject; status: string; time: integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PingSend := TPINGSend.Create;
  Memo1.Lines.Clear;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  PingSend.Free;
end;

procedure TForm1.Ping2ButtonClick(Sender: TObject);
var
  sIP: string;
  I: Integer;
begin
  PingSend.Timeout := 5;
  sIP := '192.168.0.';
  for i := 1 to 254 do
    if PingSend.Ping(sIP + IntToStr(i)) = True then
      begin
        Memo1.Lines.Add('Reply from ' + sIP + IntToStr(i) + ' in: ' + IntToStr(PingSend.PingTime) + ' ms');
      end
    else
      begin
        Memo1.Lines.Add('No response from ' + sIP + IntToStr(i) + ' in: ' + IntToStr(PingSend.Timeout) + ' ms');
      end;
end;

procedure TForm1.PingButtonClick(Sender: TObject);
begin
  Memo1.Lines.Add('Ping');
  vsSynPing1.Ping(IPEdit.Text);
end;

procedure TForm1.vsSynPing1Status(Sender: TObject; status: string; time: integer);
begin
  Memo1.Lines.Add(Status);
end;

end.

