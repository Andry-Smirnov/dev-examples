unit VersInfoListEx;

{
  Subclass that adds methods for:
    - Loading version info from Lazarus project information file (.lpi).
    - Updating version info files from project version info.
    - Updating install scripts with project version info.

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
  IniFiles,
  DOM,
  SimplerXml,
  VersInfoList,
  LConvEncoding;  {Note this is a Lazarus unit; needed for UTF8ToCP1252}

type
  TVersionInfoListEx = class(TVersionInfoList)
  public
    Target : string;
    function LoadFromLpi(const LpiFName : string) : Boolean;
    function UpdateInfoplist(const PlistFName : string;
                               out StatusMsg  : string) : Boolean;
    function UpdateVersionIni(const IniFName : string;
                                out StatusMsg  : string) : Boolean;
    function UpdateInstallScript(const ScriptFName : string;
                                   out StatusMsg   : string) : Boolean;
  end;


implementation

function TVersionInfoListEx.LoadFromLpi(const LpiFName : string) : Boolean;
 {Load version info from specified Lazarus project information file (.lpi).}
var
  Xml           : TSimplerXml;
  ConfigNode    : TDOMNode;
  ProjOptNode   : TDOMNode;
  VersInfoNode  : TDOMNode;
  MajorVers     : string;
  MinorVers     : string;
  Revision      : string;
  Build         : string;
  VersTableNode : TDOMNode;
  i             : Integer;
  CompOptNode   : TDOMNode;
  TargetNode    : TDOMNode;
begin
  Result := False;
  Clear;
  if not FileExists(LpiFName) then  {No project info file?}
    Exit;
  Xml := TSimplerXml.Create;
  try
    if not Xml.Load(LpiFName) then
      Exit;
    ConfigNode := Xml.FindNode(nil, 'CONFIG');
    if not Assigned(ConfigNode) then  {Not a Lazarus project info file?}
      Exit;
    ProjOptNode := Xml.FindNode(ConfigNode, 'ProjectOptions');
    if not Assigned(ProjOptNode) then  {File format changed?}
      Exit;
    VersInfoNode := Xml.FindNode(ProjOptNode, 'VersionInfo');
    if not Assigned(VersInfoNode) then  {File format changed?}
      Exit;
    MajorVers := Xml.GetAttrTextAsUTF8(Xml.FindNode(VersInfoNode, 'MajorVersionNr'),
                                       'Value');
    if MajorVers = '' then
      MajorVers := '0';
    MinorVers := Xml.GetAttrTextAsUTF8(Xml.FindNode(VersInfoNode, 'MinorVersionNr'),
                                       'Value');
    if MinorVers = '' then
      MinorVers := '0';
    Revision := Xml.GetAttrTextAsUTF8(Xml.FindNode(VersInfoNode, 'RevisionNr'),
                                      'Value');
    if Revision = '' then
      Revision := '0';
    Build := Xml.GetAttrTextAsUTF8(Xml.FindNode(VersInfoNode, 'BuildNr'),
                                   'Value');
    if Build = '' then
      Build := '0';
    VersTableNode := Xml.FindNode(VersInfoNode, 'StringTable');
    for i := 1 to Length(StandardVersionKeys) do
      begin
      if StandardVersionKeys[i] = 'FileVersion' then
        Add(StandardVersionKeys[i] + '=' + MajorVers + '.' + MinorVers + '.' +
            Revision + '.' + Build)
      else
        Add(StandardVersionKeys[i] + '=' + 
            Xml.GetAttrTextAsUTF8(VersTableNode, StandardVersionKeys[i]));
      end;

    Add('BundleIdentifier=' +
        Xml.GetAttrTextAsUTF8(VersTableNode, 'BundleIdentifier'));

    langID := Xml.GetAttrTextAsUTF8(Xml.FindNode(VersInfoNode, 'Language'), 'Value');
    if langID = '' then
      langID := '0409';  {U.S. English}
    charsetID := Xml.GetAttrTextAsUTF8(Xml.FindNode(VersInfoNode, 'CharSet'), 'Value');
    if charsetID = '' then
      charsetID := '04E4';  {ANSI / Latin1}

     {Get project's target too}
    CompOptNode := Xml.FindNode(ConfigNode, 'CompilerOptions');
    TargetNode := Xml.FindNode(CompOptNode, 'Target');
    Target := Xml.GetAttrTextAsUTF8(Xml.FindNode(TargetNode, 'Filename'), 'Value');

  finally
    Xml.Free;  
  end;
  Result := True;
end;  {LoadFromLpi}


function TVersionInfoListEx.UpdateInfoplist(const PlistFName : string;
                                              out StatusMsg  : string) : Boolean;
 {Update specified Info.plist file with currently loaded
   version information.}
var
  InF        : TextFile;
  OutF       : TextFile;
  InStr      : string;
  Xml        : TSimplerXml;
  PlistNode  : TDOMNode;
  DictNode   : TDOMNode;
  HasChanged : Boolean;
  i          : Integer;
  KeyNode    : TDOMNode;
  KeyStr     : string;
  ValueNode  : TDOMNode;

  procedure UpdateNode(      ANode   : TDOMNode;
                       const VersStr : AnsiString);
  begin
    if (VersStr <> '') and (VersStr <> Xml.GetNodeTextAsUTF8(ANode)) then
      begin
      Xml.SetNodeTextAsUTF8(ANode, VersStr);
      HasChanged := True;
      end;
  end;

begin
  Result := False;
  StatusMsg := 'Info.plist: Unable to update';
  if not FileExists(PlistFName) then
    Exit;

   {Since FPC XML units choke on Info.plist's DOCTYPE, just strip it out
     temporarily.}
  AssignFile(InF, PlistFName);
  AssignFile(OutF, PlistFName + '.tmp1');
  try
    Reset(InF);
  except
    Exit;
  end;
  try
    Rewrite(OutF);
  except
    CloseFile(InF);
    Exit;
  end;
  while not Eof(InF) do
    begin
    ReadLn(InF, InStr);
    if Copy(InStr, 1, 15) <> '<!DOCTYPE plist' then
      WriteLn(OutF, InStr);
    end;
  CloseFile(InF);
  CloseFile(OutF);  

  Xml := TSimplerXml.Create;
  try
    if not Xml.Load(PlistFName + '.tmp1') then
      Exit;
    PlistNode := Xml.FindNode(nil, 'plist');
    if not Assigned(PlistNode) then
      Exit;
    DictNode := Xml.FindNode(PlistNode, 'dict');
    if not Assigned(DictNode) then
      Exit;

    HasChanged := False;
    i := 0;
    while i < DictNode.ChildNodes.Count-1 do
      begin
      KeyNode := Xml.GetNode(DictNode, i);
      KeyStr := Xml.GetNodeTextAsUTF8(KeyNode);
      ValueNode := Xml.GetNode(DictNode, i+1);

      if KeyStr = 'CFBundleExecutable' then
        UpdateNode(ValueNode, Target)

      else if KeyStr = 'CFBundleName' then
        UpdateNode(ValueNode, Values['ProductName'])

      else if KeyStr = 'CFBundleIconFile' then
        UpdateNode(ValueNode, Target + '.icns')
         {Assume icon has same name as bundle executable}

      else if KeyStr = 'CFBundleShortVersionString' then
        UpdateNode(ValueNode, Values['ProductVersion'])
         {Assume this is release version}

      else if KeyStr = 'CFBundleVersion' then
        UpdateNode(ValueNode, Values['FileVersion'])
         {Assume this is build version}

      else if KeyStr = 'NSHumanReadableCopyright' then
        UpdateNode(ValueNode, Values['LegalCopyright'])

      else if KeyStr = 'CFBundleIdentifier' then
        UpdateNode(ValueNode, Values['BundleIdentifier']);

       // maybe could use langID to set CFBundleDevelopmentRegion
       //  (eg, 0409 -> en-US)

      Inc(i, 2);
      end;

    if not HasChanged then
      StatusMsg := 'Info.plist: No update needed'
    else
      begin
      if not Xml.SaveTo(PlistFName + '.tmp1') then
        Exit;

       {Add DOCTYPE line back in}
      AssignFile(InF, PlistFName + '.tmp1');
      AssignFile(OutF, PlistFName + '.tmp2');
      try
        Reset(InF);
      except
        Exit;
      end;
      try
        Rewrite(OutF);
      except
        CloseFile(InF);
        Exit;
      end;
      while not Eof(InF) do
        begin
        ReadLn(InF, InStr);
        Write(OutF, InStr, #10);  {Use proper Mac line ending}
        if Copy(InStr, 1, 5) = '<?xml' then
          Write(OutF, '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">'#10);
        end;
      CloseFile(InF);
      CloseFile(OutF);
      if DeleteFile(PlistFName) then
        RenameFile(PlistFName + '.tmp2', PlistFName)
      else
        begin
        DeleteFile(PlistFName + '.tmp2');
        Exit;
        end;
      StatusMsg := 'Info.plist: Updated';
      end;

    Result := True;

  finally
    Xml.Free;
    DeleteFile(PlistFName + '.tmp1');
  end;

end;  {UpdateInfoplist}


function TVersionInfoListEx.UpdateVersionIni(const IniFName : string;
                                               out StatusMsg  : string) : Boolean;
 {Update specified INI-type file with currently loaded
   version information.}
var
  IniFile    : TMemIniFile;
  HasChanged : Boolean;
  i          : Integer;
  ValueStr   : string;
begin
  Result := False;
  StatusMsg := ExtractFileName(IniFName) + ': Unable to update';
  if not FileExists(IniFName) then
    Exit;

  IniFile := TMemIniFile.Create(IniFName);
  try
    IniFile.CacheUpdates := True;

    HasChanged := False;
    for i := 1 to Length(StandardVersionKeys) do
      begin
      if (Values[StandardVersionKeys[i]] <> '') and
         (Values[StandardVersionKeys[i]] <>
          IniFile.ReadString('VersionInfoKeys', StandardVersionKeys[i], '')) then
        begin
        IniFile.WriteString('VersionInfoKeys', StandardVersionKeys[i],
                            Values[StandardVersionKeys[i]]);
        HasChanged := True;
        end;
      end;
    ValueStr := IntToStr(StrToIntDef('$' + langID, 0));  {Convert from hex}
    if ValueStr <> IniFile.ReadString('VersionInfo', 'Locale', '') then
      begin
      IniFile.WriteString('VersionInfo', 'Locale', ValueStr);
      HasChanged := True;
      end;
    ValueStr := IntToStr(StrToIntDef('$' + charsetID, 0));
    if ValueStr <> IniFile.ReadString('VersionInfo', 'CodePage', '') then
      begin
      IniFile.WriteString('VersionInfo', 'CodePage', ValueStr);
      HasChanged := True;
      end;
    if HasChanged then
      begin
      IniFile.UpdateFile;
      StatusMsg := ExtractFileName(IniFName) + ': Updated';
      end
    else
      StatusMsg := ExtractFileName(IniFName) + ': No update needed';
    Result := True;
  finally
    IniFile.Free;
  end;
end;  {UpdateVersionIni}


function TVersionInfoListEx.UpdateInstallScript(const ScriptFName : string;
                                                  out StatusMsg   : string) : Boolean;
 {Update specified install script with currently
   loaded version information.}
const
  IssScript = 1;
  DmgScript = 2;
  DebScript = 3;
var
  ScriptType : Integer;
  InF        : TextFile;
  OutF       : TextFile;
  HasChanged : Boolean;
  InStr      : string;
  KeyStr     : string;
  ValueStr   : string;
  VersionStr : string;

  procedure UpdateValue(  var ValStr  : string;
                        const VersStr : AnsiString);
  begin
    if (VersStr <> '') and (VersStr <> ValStr) then
      begin
      ValStr := VersStr;
      HasChanged := True;
      end;
  end;

begin
  Result := False;
  StatusMsg := ExtractFileName(ScriptFName) + ': Unable to update';
  if not FileExists(ScriptFName) then
    Exit;

  if ExtractFileExt(ScriptFName) = '.iss' then
    ScriptType := IssScript
  else if ExtractFileName(ScriptFName) = 'makedmg.sh' then
    ScriptType := DmgScript
  else if ExtractFileName(ScriptFName) = 'makedeb.sh' then
    ScriptType := DebScript
  else
    Exit;

  AssignFile(InF, ScriptFName);
  AssignFile(OutF, ScriptFName + '.tmp');
  try
    Reset(InF);
  except
    Exit;
  end;
  try
    Rewrite(OutF);
  except
    CloseFile(InF);
    Exit;
  end;

  HasChanged := False;
  while not Eof(InF) do
    begin
    ReadLn(InF, InStr);
    case ScriptType of
      IssScript :
        begin
        InStr := Trim(InStr);
        if Copy(InStr, 1, 8) = '#define ' then
          begin
          InStr := Trim(Copy(InStr, 9, MaxInt));
          if Pos(' ', InStr) > 0 then
            begin
            KeyStr := Copy(InStr, 1, Pos(' ', InStr)-1);
            ValueStr := Trim(Copy(InStr, Pos(' ', InStr)+1, MaxInt));
            if Copy(ValueStr, 1, 1) = '"' then
              ValueStr := Copy(ValueStr, 2, Length(ValueStr)-2);
            end
          else
            begin
            KeyStr := InStr;
            ValueStr := '';
            end;
           {Note: Not sure how strings should be encoded in .iss,
             so use Latin-1 for now.}
          if KeyStr = 'myAppName' then
            UpdateValue(ValueStr, UTF8ToCP1252(Values['ProductName']))
          else if KeyStr = 'myAppVersion' then
            UpdateValue(ValueStr, Values['ProductVersion'])
          else if KeyStr = 'myExeFile' then
            UpdateValue(ValueStr, UTF8ToCP1252(Target + '.exe'))
          else if KeyStr = 'myIconFile' then
            UpdateValue(ValueStr, UTF8ToCP1252(Target + '.ico'))
          else if KeyStr = 'myCopyright' then
            UpdateValue(ValueStr, UTF8ToCP1252(Values['LegalCopyright']))
          else if KeyStr = 'myCompany' then
            UpdateValue(ValueStr, UTF8ToCP1252(Values['CompanyName']));
          InStr := '#define ' + KeyStr + ' "' + ValueStr + '"';
          end;
        Write(OutF, InStr, #13#10);  {Use proper Windows line ending}
        end;

      DmgScript, DebScript :
        begin
        if (Pos('=', InStr) > 0) and
           ((Pos(' ', InStr) = 0) or (Pos(' ', InStr) > Pos('=', InStr))) then
          begin
          KeyStr := Trim(Copy(InStr, 1, Pos('=', InStr)-1));
          ValueStr := Trim(Copy(InStr, Pos('=', InStr)+1, MaxInt));
          if KeyStr = 'appname' then
            begin
            VersionStr := Values['ProductName'];
            if Pos(' ', VersionStr) > 0 then
              VersionStr := '"' + VersionStr + '"';
            UpdateValue(ValueStr, VersionStr);
            end
          else if KeyStr = 'appversion' then
            UpdateValue(ValueStr, Values['ProductVersion'])
          else if KeyStr = 'pkgversion' then
            begin
            VersionStr := Copy(Values['FileVersion'], 1, Length(Values['ProductVersion']));
            if Length(VersionStr) <> 1 then
              VersionStr := '0';
            UpdateValue(ValueStr, VersionStr);
            end
          else if (KeyStr = 'exefile') and (ScriptType = DebScript) then
            UpdateValue(ValueStr, Target);
          InStr := KeyStr + '=' + ValueStr;
          end;
        Write(OutF, InStr, #10);  {Use proper Unix line ending}
        end;
      end;
    end;
  CloseFile(InF);
  CloseFile(OutF);  

  if not HasChanged then
    begin
    DeleteFile(ScriptFName + '.tmp');
    StatusMsg := ExtractFileName(ScriptFName) + ': No update needed';
    end
  else
    begin
    if DeleteFile(ScriptFName) then
      RenameFile(ScriptFName + '.tmp', ScriptFName)
    else
      begin
      DeleteFile(ScriptFName + '.tmp');
      Exit;
      end;
    StatusMsg := ExtractFileName(ScriptFName) + ': Updated';
    end;

  Result := True;

end;  {UpdateInstallScript}


end.
