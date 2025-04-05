unit PrefsList;

{
  Class for reading and writing app preferences and settings.

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
{$ELSE}
  IniFiles;
{$ENDIF}

type
  TPreferencesList = class
  private
{$IFNDEF DARWIN}
    PrefsIni : TMemIniFile;
{$ENDIF}
    function GetValues(const Section : string;
                       const Ident   : string) : string;
    procedure SetValues(const Section : string;
                        const Ident   : string;
                        const Value   : string);
  public
    property Values[const Section : string;
                    const Ident   : string] : string read GetValues write SetValues;
    destructor Destroy; override;
    function Load(const FileName : string) : Boolean;
    function DefaultLoad(const AppName : string) : Boolean;
  end;


implementation

destructor TPreferencesList.Destroy;
begin
{$IFNDEF DARWIN}
  if Assigned(PrefsIni) then
    begin
    try
      PrefsIni.UpdateFile;
    except
    end;
    PrefsIni.Free;
    end;
{$ENDIF}
end;


function TPreferencesList.Load(const FileName : string) : Boolean;
 {Note FileName ignored with Mac.}
begin
{$IFDEF DARWIN}
  Result := True;  {Do nothing}
{$ELSE}
  Result := False;
  try
    if Assigned(PrefsIni) then
      PrefsIni.Free;
    PrefsIni := TMemIniFile.Create(FileName);
    Result := True;
  except
  end;
{$ENDIF}
end;


function TPreferencesList.DefaultLoad(const AppName : string) : Boolean;
 {Note AppName ignored with both Mac and Linux.}
{$IFNDEF DARWIN}
var
  IniFName : string;
{$ENDIF}
begin
{$IFDEF DARWIN}
  Result := True;  {Do nothing}
{$ELSE}
  Result := False;
  IniFName := ExtractFileName(ParamStr(0));
  {$IFDEF MSWINDOWS}
  IniFName := ChangeFileExt(IniFName, '.ini');
  if GetEnvironmentVariable('APPDATA') <> '' then  {Variable is set?}
    IniFName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) +
                AppName + PathDelim +
                IniFName;               
  {$ELSE}  {Linux}
  IniFName := ExpandFileName(
               '~' + PathDelim + 
               '.' + IniFName + PathDelim +
               IniFName + '.ini');
  {$ENDIF}
  Result := Load(IniFName);
{$ENDIF}
end;


function TPreferencesList.GetValues(const Section : string;
                                    const Ident   : string) : string;
begin
{$IFDEF DARWIN}
  Result := GetPrefUTF8String(Section + ':' + Ident);
{$ELSE}
  Result := '';
  if not Assigned(PrefsIni) then
    Exit;
  Result := PrefsIni.ReadString(Section, Ident, '');
{$ENDIF}
end;


procedure TPreferencesList.SetValues(const Section : string;
                                     const Ident   : string;
                                     const Value   : string);
begin
{$IFDEF DARWIN}
  SetPrefUTF8String(Section + ':' + Ident, Value);
{$ELSE}
  if not Assigned(PrefsIni) then
    Exit;
  PrefsIni.WriteString(Section, Ident, Value);
{$ENDIF}
end;


end.

