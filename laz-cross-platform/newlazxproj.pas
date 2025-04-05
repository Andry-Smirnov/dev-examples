unit NewLazXProj;

{
  Dialog that prompts for information about new cross-platform project.

  Author:     Phil Hess.
  Copyright:  Copyright (C) 2016 Phil Hess. All rights reserved.
  License:    Modified LGPL.
}

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type
  TNewLazXProjDlg = class(TForm)
    AppLbl: TLabel;
    AppEdit: TEdit;
    AppHint: TLabel;
    ProjLbl: TLabel;
    ProjEdit: TEdit;
    ProjHint: TLabel;
    BundIdLbl: TLabel;
    BundIdEdit: TEdit;
    BundIdHint: TLabel;
    DirLbl: TLabel;
    DirEdit: TEdit;
    DirBrowseBtn: TButton;
    ScriptsCheckBox: TCheckBox;
    CancelBtn: TButton;
    CreateBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AppEditExit(Sender: TObject);
    procedure DirBrowseBtnClick(Sender: TObject);
    procedure CreateBtnClick(Sender: TObject);
  private
  public
  end;

var
  NewLazXProjDlg: TNewLazXProjDlg;


implementation

{$R *.lfm}

procedure TNewLazXProjDlg.FormCreate(Sender: TObject);
{$IFNDEF DARWIN}  {Swap button positions?}
var
  SaveLeft : Integer;
begin
  if CancelBtn.Left < CreateBtn.Left then
    begin
    SaveLeft := CancelBtn.Left;
    CancelBtn.Left := CreateBtn.Left;
    CreateBtn.Left := SaveLeft;
    end;
{$ELSE}
begin
{$ENDIF}
end;


procedure TNewLazXProjDlg.AppEditExit(Sender: TObject);
var
  ProjStr : string;
begin
  AppEdit.Text := Trim(AppEdit.Text);
  if AppEdit.Text <> '' then
    begin
    ProjStr := LowerCase(AppEdit.Text);
    while Pos(' ', ProjStr) > 0 do
      Delete(ProjStr, Pos(' ', ProjStr), 1);
    if ProjEdit.Text = '' then
      ProjEdit.Text := ProjStr;
    if BundIdEdit.Text = '' then
      BundIdEdit.Text := 'com.mydomain.' + ProjStr;
    end;
end;


procedure TNewLazXProjDlg.DirBrowseBtnClick(Sender: TObject);
var
  SelectDirDlg : TSelectDirectoryDialog;
begin
  SelectDirDlg := TSelectDirectoryDialog.Create(Application);
  try
    SelectDirDlg.Options := [ofPathMustExist,ofHideReadOnly];
    SelectDirDlg.Title := 'Select directory where project files should be created';
    if SelectDirDlg.Execute then
      DirEdit.Text := SelectDirDlg.FileName
    else  {User cancelled}
      Exit;
  finally
    SelectDirDlg.Free;
    end;
end;


procedure TNewLazXProjDlg.CreateBtnClick(Sender: TObject);


  function BundIdIsValid(BundId : string) : Boolean;
  var
    i : Integer;
  begin
    Result := False;
    for i := 1 to Length(BundId) do
      begin
      if not (BundId[i] in ['A'..'Z', 'a'..'z', '0'..'9', '.', '-']) then
        Exit;
      end;
    Result := True;
  end;

begin
  ModalResult := mrNone;  {Ignore click}

  if AppEdit.Text = '' then
    begin
    MessageDlg('Application name not specified.', mtError, [mbOK], 0);
    Exit;
    end;

  if ProjEdit.Text = '' then
    begin
    MessageDlg('Project name not specified.', mtError, [mbOK], 0);
    Exit;
    end;

  if BundIdEdit.Text = '' then
    begin
    MessageDlg('Bundle identifier not specified.', mtError, [mbOK], 0);
    Exit;
    end;

  if not BundIdIsValid(BundIdEdit.Text) then
    begin
     MessageDlg('Bundle identifier is not valid - must be alphanumeric.',
                mtError, [mbOK], 0);
     Exit;
    end;

  if DirEdit.Text = '' then
    begin
    MessageDlg('Directory not selected.', mtError, [mbOK], 0);
    Exit;
    end;

  if not DirectoryExists(ExpandFileName(DirEdit.Text)) then
    begin
    MessageDlg('Directory does not exist.', mtError, [mbOK], 0);
    Exit;
    end;

  if DirectoryExists(ExpandFileName(IncludeTrailingPathDelimiter(DirEdit.Text) +
                                    ProjEdit.Text)) or
     FileExists(ExpandFileName(IncludeTrailingPathDelimiter(DirEdit.Text) +
                               ProjEdit.Text)) then
    begin
    MessageDlg('Project already exists.', mtError, [mbOK], 0);
    Exit;
    end;

  if ScriptsCheckBox.Checked then
    begin
    end;

  ModalResult := mrOK;
end;


end.

