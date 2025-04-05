unit LazXProj_Intf;

{
  Adds cross-platform application to Lazarus File | New dialog.

  Author:     Phil Hess.
  Copyright:  Copyright (C) 2016 Phil Hess. All rights reserved.
  License:    Modified LGPL.
}

{$mode delphi}

interface

uses
  SysUtils, Classes, DateUtils, FileUtil,
  Controls, Forms, Dialogs,
  LazIDEIntf, PackageIntf, ProjectIntf, Project,
  NewLazXProj;

type
  TLazXProjApplicationDescriptor = class(TProjectDescriptor)
  private
    AppToUse    : string;
    ProjToUse   : string;
    BundIdToUse : string;
    DirToUse    : string;
    AddScripts  : Boolean;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function DoInitDescriptor: TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;


var
  ProjectDescriptorLazXProjApplication : TLazXProjApplicationDescriptor;
  FileDescriptor : TProjectFileDescriptor;
  
procedure Register;


implementation

procedure Register;
begin
  ProjectDescriptorLazXProjApplication := TLazXProjApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorLazXProjApplication);
  FileDescriptor := TProjectFileDescriptor.Create;
end;


constructor TLazXProjApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'Cross-Platform Desktop App';
end;


function TLazXProjApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'Cross-Platform Desktop App';
end;


function TLazXProjApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result := 'The Lazarus Cross-Platform Project...'#13#10#13#10 +
            'Creates a cross-platform desktop app with support ' +
            'for the following on Windows, Mac and Linux:'#13#10#13#10 +
            '- App version information'#13#10 +
            '- App preferences and settings'#13#10 +
            '- App icons (.ico, .icns, .png)'#13#10 +
            '- On Mac, proper menus and app bundle'#13#10 +
            '- Install scripts for app (optional)'#13#10 +
            '- When version info is edited in IDE, Info.plist, ' +
            'install scripts, etc. will be updated automatically after ' +
            'next compile.'#13#10#13#10 +
            'Important!  If Lazarus throws a "division by zero" exception ' +
            'after creating the new project, for now just click OK ' +
            'to ignore it.'            
end;


function TLazXProjApplicationDescriptor.DoInitDescriptor: TModalResult;
begin
  Result := mrCancel;

   {How to check that AppXProj package that app will need is available?
     Doesn't show up with PackageEditingInterface.FindPackageWithname
     since only compiled, not installed.}

  NewLazXProjDlg := TNewLazXProjDlg.Create(Application);
  try
    if NewLazXProjDlg.ShowModal <> mrOK then
      Exit;
    AppToUse := NewLazXProjDlg.AppEdit.Text;
    ProjToUse := NewLazXProjDlg.ProjEdit.Text;
    BundIdToUse := NewLazXProjDlg.BundIdEdit.Text;
    DirToUse := NewLazXProjDlg.DirEdit.Text;
    AddScripts := NewLazXProjDlg.ScriptsCheckBox.Checked;
  finally
    NewLazXProjDlg.Free;
  end;

  Result := mrOK;
end;


function TLazXProjApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
begin
  inherited InitProject(AProject);

  AProject.Title := '';  {Don't let Laz set title; set from version info!}

  AProject.Flags := AProject.Flags - [pfMainUnitHasTitleStatement];

  AProject.AddPackageDependency('LCL');
  AProject.AddPackageDependency('AppXProj');

   {Always a good idea to start out with checks on}
  AProject.LazCompilerOptions.RangeChecks := True;
  AProject.LazCompilerOptions.OverflowChecks := True;
  AProject.LazCompilerOptions.UnitOutputDirectory := 'units/$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.Win32GraphicApp := True;

   {Note that in order to reference TProject, need Project unit in uses.
     This requires path to IDE, CodeTools and SynEdit units in package file.}
  TProject(AProject).UseAppBundle := True;
  TProject(AProject).ProjResources.XPManifest.UseManifest := True;
  TProject(AProject).TargetFilename := ProjToUse;

  TProject(AProject).ProjResources.VersionInfo.UseVersionInfo := True;
  TProject(AProject).ProjResources.VersionInfo.StringTable['ProductName'] := AppToUse;
  TProject(AProject).ProjResources.VersionInfo.StringTable['ProductVersion'] := '0.0.0';
  TProject(AProject).ProjResources.VersionInfo.StringTable['OriginalFilename'] :=
   ProjToUse + '.exe';  //since this is a Windows resource, use Windows extension
  TProject(AProject).ProjResources.VersionInfo.StringTable['LegalCopyright'] := 
   'Copyright © ' + IntToStr(YearOf(Now)) + ' ' +
{$IFDEF MSWINDOWS}
   GetEnvironmentVariable('USERNAME') + '.';
{$ELSE}
   GetEnvironmentVariable('USER') + '.';
{$ENDIF}
  TProject(AProject).ProjResources.VersionInfo.StringTable['BundleIdentifier'] := BundIdToUse;

  TProject(AProject).CompilerOptions.ExecuteAfter.Command :=
   '"$PKGDIR(AppXProj)' + PathDelim + '..' + PathDelim + 'utils' + PathDelim + 
   'fix_xproj$ExeExt()" "$Project(InfoFile)"';
   {Include path to helper app - assume it's in package's folder}

//  TProject(AProject).CompilerOptions.ExecuteAfter.ScanForFPCMessages := True;
  TProject(AProject).CompilerOptions.ExecuteAfter.ShowAllMessages := True;
   {Checking "Show all messages" will display info and error messages.}

  Result := mrOK;
end;  {InitProject}


function TLazXProjApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
var
  PackagePath   : string;
  TemplatesPath : string;
  IconsPath     : string;
  ScriptsPath   : string;
  InF           : TextFile;
  OutF          : TextFile;
  LineCnt       : Integer;
  InStr         : string;
  IconStream    : TMemoryStream;
begin
  Result := mrCancel;

  PackagePath := PackageEditingInterface.FindPackageWithname('LazXProj', nil).DirectoryExpanded;
  TemplatesPath := IncludeTrailingPathDelimiter(PackagePath + 'templates');
  IconsPath := IncludeTrailingPathDelimiter(PackagePath + 'icons');
  ScriptsPath := IncludeTrailingPathDelimiter(PackagePath + 'scripts');

  DirToUse := IncludeTrailingPathDelimiter(ExpandFileName(DirToUse));

   //check that template files exist; also check CopyFile result to see if successful
  CopyFile(TemplatesPath + 'project.lpr', DirToUse + ProjToUse + '.tmp');
  CopyFile(TemplatesPath + 'mainform.pas', DirToUse + 'mainform.pas', True);
  CopyFile(TemplatesPath + 'mainform.lfm', DirToUse + 'mainform.lfm', True);
  CopyFile(TemplatesPath + 'Info.plist', DirToUse + 'Info.plist', True);
  CopyFile(TemplatesPath + 'lazxproj.version', DirToUse + ProjToUse + '.version', True);

   {Patch first line of program file}
  AssignFile(InF, DirToUse + ProjToUse + '.tmp');
  Reset(InF);
  AssignFile(OutF, DirToUse + ProjToUse + '.lpr');
  Rewrite(OutF);
  LineCnt := 0;
  while not Eof(InF) do
    begin
    Inc(LineCnt);
    ReadLn(InF, InStr);
    if LineCnt = 1 then
      WriteLn(OutF, 'program ' + ProjToUse + ';')
    else
      WriteLn(OutF, InStr);
    end;
  CloseFile(InF);
  CloseFile(OutF);
  DeleteFile(DirToUse + ProjToUse + '.tmp');

  LazarusIDE.DoNewEditorFile(FileDescriptor, ProjToUse + '.lpr',
                             '', [nfIsPartOfProject,nfOpenInEditor]);
  AProject.MainFileID := 0;  {Laz won't try to compile without this}

  LazarusIDE.DoNewEditorFile(FileDescriptor, 'mainform.pas',
                             '', [nfIsPartOfProject,nfOpenInEditor]);

   //check that icon files exist; also check CopyFile result to see if successful
  CopyFile(IconsPath + 'lazxproj.ico', DirToUse + ProjToUse + '.ico', True);
  CopyFile(IconsPath + 'lazxproj.icns', DirToUse + ProjToUse + '.icns', True);
  CopyFile(IconsPath + 'lazxproj.png', DirToUse + ProjToUse + '.png', True);

  IconStream := TMemoryStream.Create;
  try
    IconStream.LoadFromFile(DirToUse + ProjToUse + '.ico');
    TProject(AProject).ProjResources.ProjectIcon.SetStream(IconStream);
  finally
    IconStream.Free;
  end;

  if AddScripts then
    begin
     //check that script files exist; also check CopyFile result to see if successful
    CopyFile(ScriptsPath + 'lazxproj.iss', DirToUse + ProjToUse + '.iss', True);
    CopyFile(ScriptsPath + 'makedmg.sh', DirToUse + 'makedmg.sh', True);
    CopyFile(ScriptsPath + 'makedeb.sh', DirToUse + 'makedeb.sh', True);
    end;

  TProject(AProject).WriteProject([], DirToUse + ProjToUse + '.lpi', nil);  //check result?

   //Causes division-by-0 error, but if ignore it can continue; doesn't appear 
   // to be any other way of opening project we've just created.
  LazarusIDE.DoCloseProject;
  LazarusIDE.DoOpenProjectFile(DirToUse + ProjToUse + '.lpi', [ofAddToRecent]);

  Result := mrOK;
end;  {CreateStartFiles}


end.
