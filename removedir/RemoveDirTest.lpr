program RemoveDirTest;


uses
  SysUtils,
  FileUtil,
  LazFileUtils;


var
  DirName: string;
  IsDelete: Boolean;


begin
  DirName := 'base';
  IsDelete := DeleteDirectory( DirName, False );//True );
//  if IsDelete then
//    IsDelete := RemoveDirUTF8(DirName);
//    IsDelete := DeleteDirectory(DirName, False);
  if IsDelete then
    WriteLn(Format('Directory "%s" with file and subdirectories removed', [DirName]))
  else
    WriteLn('Shit');
end.

