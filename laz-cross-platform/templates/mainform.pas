unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  LCLIntf, LCLType,
  VersInfoList, PrefsList;

type
  TMainForm = class(TForm)
    MainMenu      : TMainMenu;

    FileMenu      : TMenuItem;
    FileNewCmd    : TMenuItem;
    FileOpenCmd   : TMenuItem;
    FileRecentCmd : TMenuItem;

    HelpMenu      : TMenuItem;
    HelpHelpCmd   : TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure AboutCmdClick(Sender: TObject);

{$IFNDEF DARWIN}
    procedure FileExitCmdClick(Sender: TObject);
{$ENDIF}

    procedure HelpHelpCmdClick(Sender: TObject);

  private
{$IFDEF DARWIN}
    AppMenu      : TMenuItem;
    AppAboutCmd  : TMenuItem;
{$ENDIF}
{$IFNDEF DARWIN}
    FileSep1Cmd  : TMenuItem;
    FileExitCmd  : TMenuItem;
    HelpAboutCmd : TMenuItem;
{$ENDIF}

  public
    VersionList    : TVersionInfoList;
    ProgramName    : string;
    ProgramVersion : string;

    PrefsList      : TPreferencesList;

    procedure SetupMenu;
    procedure LoadPreferences;
    procedure SavePreferences;
    procedure CheckOKCancelBtns(OKBtn     : TControl;
                                CancelBtn : TControl);

  end;

var
  MainFrm: TMainForm;


implementation

{$R *.lfm}


procedure TMainForm.FormCreate(Sender: TObject);
begin
  VersionList := TVersionInfoList.Create;
  VersionList.DefaultLoad;
  ProgramName := VersionList.Values['ProductName'];
  ProgramVersion := VersionList.Values['ProductVersion'];

  PrefsList := TPreferencesList.Create;
  PrefsList.DefaultLoad(ProgramName);
  LoadPreferences;

  Application.Title := ProgramName;

  SetupMenu;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SavePreferences;
  PrefsList.Free;
  VersionList.Free;
end;


procedure TMainForm.AboutCmdClick(Sender: TObject);
begin
  MessageDlg(ProgramName + ', version ' + ProgramVersion + #10#10 +
             VersionList.Values['FileDescription'] + #10#10 +
             VersionList.Values['LegalCopyright'],
             mtInformation, [mbOK], 0);
  //TODO: Proper About box.
end;


{$IFNDEF DARWIN}
procedure TMainForm.FileExitCmdClick(Sender: TObject);
begin
  Close;
end;
{$ENDIF}


procedure TMainForm.HelpHelpCmdClick(Sender: TObject);
begin
  MessageDlg('Help!', mtInformation, [mbOK], 0);
  //TODO: Proper help.
end;


procedure TMainForm.SetupMenu;
begin
{$IFDEF DARWIN}
  AppMenu := TMenuItem.Create(Self);  {Application menu}
  AppMenu.Caption := #$EF#$A3#$BF;  {Unicode Apple logo char}
  MainMenu.Items.Insert(0, AppMenu);

  AppAboutCmd := TMenuItem.Create(Self);
  AppAboutCmd.Caption := 'About ' + ProgramName;
  AppAboutCmd.OnClick := {$IFDEF FPC_OBJFPC}@{$ENDIF}AboutCmdClick;
  AppMenu.Add(AppAboutCmd);  {Add About as item in application menu}
{$ENDIF}

{$IFNDEF DARWIN}
  FileNewCmd.ShortCut := ShortCut(VK_N, [ssCtrl]);
  FileOpenCmd.ShortCut := ShortCut(VK_O, [ssCtrl]);

  FileSep1Cmd := TMenuItem.Create(Self);
  FileSep1Cmd.Caption := '-';
  FileMenu.Add(FileSep1Cmd);

  FileExitCmd := TMenuItem.Create(Self);
  FileExitCmd.Caption := 'Exit';
  FileExitCmd.OnClick := {$IFDEF FPC_OBJFPC}@{$ENDIF}FileExitCmdClick;
  FileMenu.Add(FileExitCmd);

  HelpAboutCmd := TMenuItem.Create(Self);
  HelpAboutCmd.Caption := 'About ' + ProgramName;
  HelpAboutCmd.OnClick := {$IFDEF FPC_OBJFPC}@{$ENDIF}AboutCmdClick;
  HelpMenu.Add(HelpAboutCmd);
{$ENDIF}

  HelpHelpCmd.Caption := ProgramName + ' Help';
end;


procedure TMainForm.LoadPreferences;
begin
  Left := StrToIntDef(PrefsList.Values['Startup', 'Left'], Left);
  Top := StrToIntDef(PrefsList.Values['Startup', 'Top'], Top);
  Width := StrToIntDef(PrefsList.Values['Startup','Width'], Width);
  Height := StrToIntDef(PrefsList.Values['Startup', 'Height'], Height);
  if PrefsList.Values['Startup', 'Maximized'] = 'True' then
    begin
    Position := poDesigned;
    WindowState := wsMaximized;
    end;
end;


procedure TMainForm.SavePreferences;
var
  IsMaxStr : string;
begin
  PrefsList.Values['Startup', 'Left'] := IntToStr(Left);
  PrefsList.Values['Startup', 'Top'] := IntToStr(Top);
  PrefsList.Values['Startup', 'Width'] := IntToStr(Width);
  PrefsList.Values['Startup', 'Height'] := IntToStr(Height);
  IsMaxStr := 'False';
  if WindowState = wsMaximized then
    IsMaxStr := 'True';
  PrefsList.Values['Startup', 'Maximized'] := IsMaxStr;  
end;


procedure TMainForm.CheckOKCancelBtns(OKBtn     : TControl;
                                      CancelBtn : TControl);
 {Swap OK and Cancel button positions on Mac.
  Call this with a dialog's buttons prior to ShowModal.}
{$IFDEF DARWIN}
var
  SaveLeft : Integer;
begin
  if OKBtn.Left < CancelBtn.Left then
    begin
    SaveLeft := OKBtn.Left;
    OKBtn.Left := CancelBtn.Left;
    CancelBtn.Left := SaveLeft;
    end;
{$ELSE}  {Do nothing}
begin
{$ENDIF}
end;


end.
