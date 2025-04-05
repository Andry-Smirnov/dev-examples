program Fix_XProj;

{
  Fix up cross-platform version info files, etc.

  Typically this console program is run following project compilation.

  Author:    Phil Hess.
  Copyright: Copyright 2016 Phil Hess.
  License:   Modified LGPL (see Free Pascal's rtl/COPYING.FPC).
             This means you can link your code to this compiled unit (statically 
             in a standalone executable or dynamically in a library) without 
             releasing your code. Only changes to this unit need to be made 
             publicly available.
}

{$mode delphi}

uses
  SysUtils,
  FileUtil,  {Note this is a Lazarus unit; needed for CopyFile}
{$IFDEF UNIX}
  BaseUnix,
{$ENDIF}
  VersInfoListEx;


procedure DoUpdates(const ProjLpiFName : string);
var
  VersInfo      : TVersionInfoListEx;
  ProjPath      : string;
  PlistFName    : string;
  StatusMsg     : string;
  UpdateOkay    : Boolean;
{$IFDEF DARWIN}
  AppBundlePath : string;
{$ENDIF}
  IniFName      : string;
  ScriptFName   : string;
begin
  VersInfo := TVersionInfoListEx.Create;
  try
    if not VersInfo.LoadFromLpi(ProjLpiFName) then
      begin
      WriteLn('Unable to load project file: ', ProjLpiFName);
      Exit;
      end;

    ProjPath := ExtractFilePath(ProjLpiFName);

    PlistFName := ProjPath + 'Info.plist';
    if not FileExists(PlistFName) then
      WriteLn('Info.plist: No file to update')
    else
      begin
      UpdateOkay := VersInfo.UpdateInfoPlist(PlistFName, StatusMsg);
      WriteLn(StatusMsg);
      if UpdateOkay then
        begin
{$IFDEF DARWIN}
        AppBundlePath := ProjPath + VersInfo.Target + '.app';
        if DirectoryExists(AppBundlePath) and
           (VersInfo.Values['ProductName'] <> '') then
          begin
          RenameFile(AppBundlePath, ProjPath + VersInfo.Values['ProductName'] + '.app');
          if (not SameText(VersInfo.Target, VersInfo.Values['ProductName'])) and
             (not DirectoryExists(AppBundlePath)) then
            fpSymlink(PChar(VersInfo.Values['ProductName'] + '.app'),
                      PChar(AppBundlePath));
             {Lazarus assumes that app bundle has same name as target
               executable (case not important apparently) and can't debug
               otherwise, so create a symlink to actual bundle using name
               that Laz expects.}
          end;
        AppBundlePath := ProjPath + VersInfo.Values['ProductName'] + '.app';
        if DirectoryExists(AppBundlePath) then
          begin
          CopyFile(PlistFName, AppBundlePath + '/Contents/Info.plist', True);
          if FileExists(ProjPath + VersInfo.Target) then
            begin
            DeleteFile(AppBundlePath + '/Contents/MacOS/' + VersInfo.Target);
            CopyFile(ProjPath + VersInfo.Target, 
                     AppBundlePath + '/Contents/MacOS/' + VersInfo.Target, True);
            FpChMod(AppBundlePath + '/Contents/MacOS/' + VersInfo.Target,
                    S_IRUSR or S_IXUSR or S_IWUSR or
                    S_IRGRP or S_IXGRP or
                    S_IROTH or S_IXOTH);
            end;
          if FileExists(ProjPath + VersInfo.Target + '.icns') then
            CopyFile(ProjPath + VersInfo.Target + '.icns',
                     AppBundlePath + '/Contents/Resources/' + VersInfo.Target + '.icns', True);
          end;
{$ENDIF}
        end;
      end;

    IniFName := ProjPath + VersInfo.Target + '.version';
    if FileExists(IniFName) then
      begin
      VersInfo.UpdateVersionIni(IniFName, StatusMsg);
      WriteLn(StatusMsg);
      end;

    ScriptFName := ProjPath + VersInfo.Target + '.iss';
    if FileExists(ScriptFName) then
      begin
      VersInfo.UpdateInstallScript(ScriptFName, StatusMsg);
      WriteLn(StatusMsg);
      end;

    ScriptFName := ProjPath + 'makedmg.sh';
    if FileExists(ScriptFName) then
      begin
      VersInfo.UpdateInstallScript(ScriptFName, StatusMsg);
{$IFDEF DARWIN}
      FpChMod(ScriptFName,
              S_IRUSR or S_IXUSR or S_IWUSR or
              S_IRGRP or S_IXGRP or
              S_IROTH or S_IXOTH);
{$ENDIF}
      WriteLn(StatusMsg);
      end;

    ScriptFName := ProjPath + 'makedeb.sh';
    if FileExists(ScriptFName) then
      begin
      VersInfo.UpdateInstallScript(ScriptFName, StatusMsg);
{$IFDEF LINUX}
      FpChMod(ScriptFName,
              S_IRUSR or S_IXUSR or S_IWUSR or
              S_IRGRP or S_IXGRP or
              S_IROTH or S_IXOTH);
{$ENDIF}
      WriteLn(StatusMsg);
      end;

  finally
    VersInfo.Free;
  end;
end;  {DoUpdates}


var
  ProjLpiFName : string;
begin
  if ParamCount = 0 then
    begin
    WriteLn('Usage: fix_xproj projectfile');
    WriteLn('Example: fix_xproj myproj.lpi');
    Exit;
    end;

  ProjLpiFName := ParamStr(1);
  if not FileExists(ProjLpiFName) then
    begin
    WriteLn(ProjLpiFName, ' does not exist');
    Exit;
    end;

  DoUpdates(ExpandFileName(ProjLpiFName));
end.
