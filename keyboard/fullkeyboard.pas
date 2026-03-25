unit fullkeyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tfullkeyboardform }

  Tfullkeyboardform = class(TForm)
    keybtna: TButton;
    keybtn3: TButton;
    keybtn8: TButton;
    keybtn9: TButton;
    keybtn0: TButton;
    keybtn1: TButton;
    keybtn4: TButton;
    keybtn5: TButton;
    keybtn7: TButton;
    keybtn2: TButton;
    keybtn6: TButton;
    keybntnewline: TButton;
    keybtnupcapslock: TButton;
    keybtnupcasel: TButton;
    keybtnclose: TButton;
    keybtnz: TButton;
    keybtnq: TButton;
    keybtnstroke: TButton;
    keybtnp: TButton;
    keybtnc: TButton;
    keybtne: TButton;
    keybtnv: TButton;
    keybtnr: TButton;
    keybtnb: TButton;
    keybtnt: TButton;
    keybtnn: TButton;
    keybtny: TButton;
    keybtnm: TButton;
    keybtnu: TButton;
    keybtncomma: TButton;
    keybtni: TButton;
    keybtndot: TButton;
    keybtno: TButton;
    keybtnaa: TButton;
    keybtns: TButton;
    keybtnoe: TButton;
    keybtnx: TButton;
    keybtnw: TButton;
    keybtnsdab: TButton;
    keybtnd: TButton;
    keybtnf: TButton;
    keybtng: TButton;
    keybtnh: TButton;
    keybtnj: TButton;
    keybtnk: TButton;
    keybtnl: TButton;
    keybtnae: TButton;
    keybtnspace: TButton;
    keybtnback: TButton;
    keybtnupcaser: TButton;

    procedure FormCreate(Sender: TObject);
    procedure keybntnewlineClick(Sender: TObject);
    procedure keybtnbackClick(Sender: TObject);
    procedure keybtnaClick(Sender: TObject);
    procedure changecaption;
    procedure keybtncloseClick(Sender: TObject);
    procedure keybtnupcapslockClick(Sender: TObject);
    procedure keybtnupcaserClick(Sender: TObject);
  private
    resulttxt: string;
    AResEdit: TCustomEdit;
  public
    procedure ExecuteEdit(AnEdit: TCustomEdit);
  end;


var
  fullkeyboardform: Tfullkeyboardform;
  keycase: string;
  upcase: boolean;
  callertype: string;


implementation


{$R *.lfm}



{ Tfullkeyboardform }

procedure Tfullkeyboardform.FormCreate(Sender: TObject);
begin
  keycase := 'lower';
  upcase := False;
  fullkeyboardform.Left := 0;
  fullkeyboardform.Top := 480 - fullkeyboardform.Height;
end;

procedure Tfullkeyboardform.keybntnewlineClick(Sender: TObject);
begin
  if callertype = 'edit' then
    begin
      AResEdit.Text := resulttxt;
      fullkeyboardform.Visible := False;
    end
  else
    begin
      resulttxt := resulttxt + chr(13);
      AResEdit.Text := resulttxt;
    end;
end;

procedure Tfullkeyboardform.keybtnupcapslockClick(Sender: TObject);
begin
  changecaption;
end;

procedure Tfullkeyboardform.keybtnupcaserClick(Sender: TObject);
begin
  keycase := 'lower';
  changecaption;
  upcase := True;
end;

procedure Tfullkeyboardform.ExecuteEdit(AnEdit: TCustomEdit);
begin
  resulttxt := AnEdit.Text;
  AResEdit := AnEdit;
  if not Visible then Show;
  BringToFront;
  if AnEdit.InheritsFrom(TCustomMemo) then
    callertype := 'memo'
  else
    callertype := 'edit';
  //  keybntnewline.Enabled:=false;
end;

procedure Tfullkeyboardform.keybtnaClick(Sender: TObject);
begin
  resulttxt := resulttxt + TButton(Sender).Caption;
  AResEdit.Text := resulttxt;
  if upcase = True then
    begin
      upcase := False;
      keycase := 'upper';
      changecaption;
      keycase := 'lower';
    end;
end;

procedure Tfullkeyboardform.keybtnbackClick(Sender: TObject);
begin
  Delete(resulttxt, length(resulttxt), 1);
  AResEdit.Text := resulttxt;
end;

procedure Tfullkeyboardform.changecaption;
begin
  if keycase = 'lower' then
    begin
      keybtna.Caption := 'A';
      keybtnz.Caption := 'Z';
      keybtnq.Caption := 'Q';
      keybtnp.Caption := 'P';
      keybtnc.Caption := 'C';
      keybtne.Caption := 'E';
      keybtnv.Caption := 'V';
      keybtnr.Caption := 'R';
      keybtnb.Caption := 'B';
      keybtnt.Caption := 'T';
      keybtnn.Caption := 'N';
      keybtny.Caption := 'Y';
      keybtnm.Caption := 'M';
      keybtnu.Caption := 'U';
      keybtncomma.Caption := ';';
      keybtni.Caption := 'I';
      keybtndot.Caption := ':';
      keybtno.Caption := 'O';
      keybtnaa.Caption := 'Å';
      keybtns.Caption := 'S';
      keybtnoe.Caption := 'Ø';
      keybtnx.Caption := 'X';
      keybtnw.Caption := 'W';
      keybtnsdab.Caption := '*';
      keybtnd.Caption := 'D';
      keybtnf.Caption := 'F';
      keybtng.Caption := 'G';
      keybtnh.Caption := 'H';
      keybtnj.Caption := 'J';
      keybtnk.Caption := 'K';
      keybtnl.Caption := 'L';
      keybtnae.Caption := 'Æ';
      keycase := 'upper';
    end
  else
    begin
      keybtna.Caption := 'a';
      keybtnz.Caption := 'z';
      keybtnq.Caption := 'q';
      keybtnp.Caption := 'p';
      keybtnc.Caption := 'c';
      keybtne.Caption := 'e';
      keybtnv.Caption := 'v';
      keybtnr.Caption := 'r';
      keybtnb.Caption := 'b';
      keybtnt.Caption := 't';
      keybtnn.Caption := 'n';
      keybtny.Caption := 'y';
      keybtnm.Caption := 'm';
      keybtnu.Caption := 'u';
      keybtncomma.Caption := ',';
      keybtni.Caption := 'i';
      keybtndot.Caption := '.';
      keybtno.Caption := 'o';
      keybtnaa.Caption := 'å';
      keybtns.Caption := 's';
      keybtnoe.Caption := 'ø';
      keybtnx.Caption := 'x';
      keybtnw.Caption := 'w';
      keybtnsdab.Caption := '''';
      keybtnd.Caption := 'd';
      keybtnf.Caption := 'f';
      keybtng.Caption := 'g';
      keybtnh.Caption := 'h';
      keybtnj.Caption := 'j';
      keybtnk.Caption := 'k';
      keybtnl.Caption := 'l';
      keybtnae.Caption := 'æ';
      keycase := 'lower';
    end;
end;

procedure Tfullkeyboardform.keybtncloseClick(Sender: TObject);
begin
  fullkeyboardform.Visible := False;
end;


end.
