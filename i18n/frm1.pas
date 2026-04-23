unit frm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DefaultTranslator, LCLTranslator;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnMessage: TButton;
    BtnGreet: TButton;
    BtnEnglish: TButton;
    BtnSpanish: TButton;
    BtnBangla: TButton;
    EdtName: TEdit;
    EdtCountry: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure BtnBanglaClick(Sender: TObject);
    procedure BtnGreetClick(Sender: TObject);
    procedure BtnEnglishClick(Sender: TObject);
    procedure BtnMessageClick(Sender: TObject);
    procedure BtnSpanishClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Form1: TForm1;

resourcestring
  HelloMessage = 'Hello World!';
  CloseMessage = 'Closing your app... bye bye!';
  GreetMessage = 'Hey %0:s from %1:s country!';

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BtnMessageClick(Sender: TObject);
begin
  ShowMessage(HelloMessage);
end;

procedure TForm1.BtnSpanishClick(Sender: TObject);
begin
  SetDefaultLang('es');
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ShowMessage(CloseMessage);
end;

procedure TForm1.BtnGreetClick(Sender: TObject);
begin
  ShowMessage( format( GreetMessage, [EdtName.Text, EdtCountry.Text] ) );
end;

procedure TForm1.BtnBanglaClick(Sender: TObject);
begin
  SetDefaultLang('bn');
end;

procedure TForm1.BtnEnglishClick(Sender: TObject);
begin
  SetDefaultLang('en');
end;

end.

