//https://forum.lazarus.freepascal.org/index.php?topic=61215.0
program showcpuandmem;

{$mode objfpc}{$h+}
{$ifndef unix}
{$error this is Unix flavors only}
{$endif}

uses
  classes;

var
  // this is a later edit, it read Tstringlist, but declaring it like this you can put the result in any Tstrings descendent
  Mem: TStrings;
  CPU: TStrings;
begin
  Mem := TStringlist.Create;
  try
    // edit: contained a typo
    Mem.LoadFromFile('/proc/Meminfo');
    WriteLn(Mem.Text);
  finally
    Mem.Free;
  end;
  CPU := TStringlist.Create;
  try
    CPU.LoadFromFile('/proc/cpuinfo');
    WriteLn(CPU.Text);
  finally
    CPU.Free;
  end;
end.