[Setup]
#define myAppName ""
#define myAppVersion ""
#define myExeFile ""
#define myIconFile ""
#define myCopyright ""
#define myCompany "MyCompany"

; Note: Uncomment and edit these two defines and other lines below
;  to associate a file type with app.
;#define myExtension ""
;#define myFileType ""

AppName={#myAppName}
AppVersion={#myAppVersion}
AppPublisher={#myCompany}
AppVerName={#myAppName} {#myAppVersion}

DefaultDirName={pf}\{#myAppName}
DefaultGroupName={#myAppName}
UninstallDisplayIcon={app}\{#myExeFile}
SetupIconFile={#myIconFile}

;ChangesAssociations=yes

;InfoBeforeFile=readme.rtf

Compression=lzma
SolidCompression=yes

OutputDir=.
OutputBaseFilename=Setup-{#myAppName}-{#myAppVersion}

VersionInfoVersion={#myAppVersion}
VersionInfoCompany={#myCompany}
VersionInfoCopyright={#myCopyright}
VersionInfoDescription={#myAppName} single-file installer

PrivilegesRequired=admin

[Files]
Source: "{#myExeFile}"; DestDir: "{app}"
;Source: "readme.rtf"; DestDir: "{app}"

[Icons]
;Name: "{group}\Read Me"; Filename: "{app}\readme.rtf"; WorkingDir: "{app}"
Name: "{group}\{#myAppName}"; Filename: "{app}\{#myExeFile}"; WorkingDir: "{app}"
Name: "{commondesktop}\{#myAppName}"; Filename: "{app}\{#myExeFile}"; WorkingDir: "{app}"

[Registry]
;Root: HKCR; Subkey: "{#myExtension}"; ValueType: string; ValueName: ""; ValueData: "{#myFileType}"; Flags: uninsdeletekey
;Root: HKCR; Subkey: "{#myFileType}"; ValueType: string; ValueName: ""; ValueData: "{#myAppName} file"; Flags: uninsdeletekey
;Root: HKCR; Subkey: "{#myFileType}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#myExeFile},0"
;Root: HKCR; Subkey: "{#myFileType}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#myExeFile}"" ""%1"""

Root: HKLM; Subkey: "SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\{#myExeFile}"; ValueType: string; ValueName: ""; ValueData: "{app}\{#myExeFile}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\{#myExeFile}"; ValueType: string; ValueName: "Path"; ValueData: "{app}"

Root: HKLM; Subkey: "SOFTWARE\{#myCompany}\{#myAppName}"; ValueType: string; ValueName: ""; ValueData: ""; Flags: uninsdeletekey
Root: HKLM; Subkey: "SOFTWARE\{#myCompany}\{#myAppName}\{#myAppVersion}"; ValueType: string; ValueName: "InstallDir"; ValueData: "{app}"; Flags: uninsdeletekey
Root: HKLM; Subkey: "SOFTWARE\{#myCompany}\{#myAppName}\{#myAppVersion}"; ValueType: string; ValueName: "Version"; ValueData: "{#myAppVersion}"

