unit VersInfoList;

{
  Load string list with program's version information.

  Author:    Phil Hess.
  Copyright: Copyright 2016 Phil Hess.
  License:   Modified LGPL (see Free Pascal's rtl/COPYING.FPC).
             This means you can link your code to this compiled unit (statically 
             in a standalone executable or dynamically in a library) without 
             releasing your code. Only changes to this unit need to be made 
             publicly available.
}

{$mode delphi}

interface

uses
  SysUtils,
  Classes,
{$IFDEF DARWIN}
  NSMisc;
{$ENDIF}
{$IFDEF MSWINDOWS}
  fileinfo;
{$ENDIF}
{$IFDEF LINUX}
  IniFiles;
{$ENDIF}

type
  TVersionInfoList = class(TStringList)
  public
    langID    : string;  {4 hex digits: LCID; locale}
    charsetID : string;  {4 hex digits; codepage}
    procedure Clear; override;
    function Load(const FileName : string) : Boolean;
    function DefaultLoad : Boolean;
  end;

const
  StandardVersionKeys : array [1..10] of string =
   ('Comments',
    'CompanyName',
    'FileDescription',
    'FileVersion',
    'InternalName',
    'LegalCopyright',
    'LegalTrademarks',
    'OriginalFilename',
    'ProductName',
    'ProductVersion');
    

implementation

procedure TVersionInfoList.Clear;
begin
  inherited Clear;
  langID := '';
  charsetID := '';
end;


function TVersionInfoList.Load(const FileName : string) : Boolean;
{$IFDEF DARWIN}
var
  BundleId : string;
begin  {Note any FileName passed is ignored}
  Clear;
  BundleId := GetInfoPlistUTF8String('CFBundleIdentifier');
  Add('Comments=' + GetInfoPlistUTF8String(BundleId + '.Comments'));
  Add('CompanyName=' + GetInfoPlistUTF8String(BundleId + '.CompanyName'));
  Add('FileDescription=' + GetInfoPlistUTF8String(BundleId + '.FileDescription'));
  Add('FileVersion=' + GetInfoPlistUTF8String('CFBundleVersion'));  {Build version}
  Add('InternalName=' + GetInfoPlistUTF8String('CFBundleExecutable'));
  Add('LegalCopyright=' +  GetInfoPlistUTF8String('NSHumanReadableCopyright'));
  Add('LegalTrademark=');
  Add('OriginalFilename=' + GetInfoPlistUTF8String('CFBundleExecutable'));  {Same as InternalName}
  Add('ProductName=' + GetInfoPlistUTF8String('CFBundleName'));
  Add('ProductVersion=' + GetInfoPlistUTF8String('CFBundleShortVersionString'));  {Release version}

  Add('BundleIdentifier=' + GetInfoPlistUTF8String('CFBundleIdentifier'));

  langID := '';  //even though this is for version strings, maybe could use
                 // CFBundleDevelopmentRegion as proxy (eg, en-US -> 0409)

  charsetID := '';
  Result := True;
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  FileVerInfo : TFileVersionInfo;
  i           : Integer;
begin
  Result := False;
  if not FileExists(FileName) then  {No program file?}
    Exit;
  Clear;
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := FileName;
    FileVerInfo.ReadFileInfo;
    for i := 0 to FileVerInfo.VersionStrings.Count-1 do
      Add(FileVerInfo.VersionStrings[i]);
    langID := Copy(FileVerInfo.Translation, 1, 4);
    charsetID := Copy(FileVerInfo.Translation, 5, 4);
  finally
    FileVerInfo.Free;
  end;
  Result := True;
{$ENDIF}

{$IFDEF LINUX}
var
  IniFile  : TIniFile;
  SectStrs : TStringList;
  i        : Integer;
begin
  Result := False;
  if not FileExists(FileName) then  {No version file?}
    Exit;
  Clear;
  IniFile := TIniFile.Create(FileName);
  SectStrs := TStringList.Create;
  try  {Assume format similar to how stored in Delphi project files (.dof)}
    IniFile.ReadSectionValues('VersionInfoKeys', SectStrs);
    for i := 0 to SectStrs.Count-1 do
      Add(SectStrs[i]);
    langID := IniFile.ReadString('VersionInfo', 'Locale', '');
    if langID <> '' then
      langID := IntToHex(StrToIntDef(langID, 0), 4);
    charsetID := IniFile.ReadString('VersionInfo', 'CodePage', '');
    if charsetID <> '' then
      charsetID := IntToHex(StrToIntDef(charsetID, 0), 4);
  finally
    IniFile.Free;
    SectStrs.Free;
  end;
  Result := True;
{$ENDIF}
end;


function TVersionInfoList.DefaultLoad : Boolean;
 {Attempt to load version info from default locations.}
begin
{$IFDEF DARWIN}
   {Load version info from app bundle's Info.plist file}
  Result := Load('');
{$ENDIF}
{$IFDEF MSWINDOWS}
   {Load version info from resource in program's .exe file}
  Result := Load(ParamStr(0));
{$ENDIF}
{$IFDEF LINUX}
   {Load version info from INI-type file with same name as program}
  Result := Load(ChangeFileExt(ParamStr(0), '.version'));
{$ENDIF}
end;


end.
