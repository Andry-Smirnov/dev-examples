unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType,
  UniqueInstance,
  PythonEngine
  //, PythonGUIInputOutput
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    edConsole: TComboBox;
    memoConsole: TMemo;
    PythonEngine1: TPythonEngine;
    PythonInputOutput1: TPythonInputOutput;
    UniqueInstance1: TUniqueInstance;
    procedure edConsoleKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure PythonEngine1AfterInit(Sender: TObject);
    procedure PythonInputOutput1SendData(Sender: TObject; const Data: AnsiString);
    procedure PythonInputOutput1SendUniData(Sender: TObject;
      const Data: UnicodeString);
  private
  public
  end;


var
  Form1: TForm1;


implementation

{$R *.lfm}


const
  S_PYTHON_MAYJOR_VERSION = '3';
{$IFDEF WINDOWS}
  S_PYTHON_WINDOWS_MINOR_VERSION = '12';
  S_PYTHON_LIBRARY_WINDOWS = 'python' + S_PYTHON_MAYJOR_VERSION +
    S_PYTHON_WINDOWS_MINOR_VERSION + '.dll';
{$ELSE}
  S_PYTHON_NIX_MINOR_VERSION = '7';
  {$IFDEF LINUX}
  S_PYTHON_LIBRARY_LINUX = 'libpython' + S_PYTHON_MAYJOR_VERSION + '.' +
    S_PYTHON_NIX_MINOR_VERSION + 'm.so.1.0';
  {$ELSE}
    {$IFDEF DARWIN}
  S_PYTHON_LIBRARY_MACOS = '/Library/Frameworks/Python.framework/Versions/' +
    S_PYTHON_MAYJOR_VERSION + '.' + S_PYTHON_NIX_MINOR_VERSION + '/lib/libpython' +
    S_PYTHON_MAYJOR_VERSION + '.' + S_PYTHON_NIX_MINOR_VERSION + '.dylib';
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


{ TForm1 ----------------------------------------------------------------------}

procedure Py_SetSysPath(const Dirs: array of string);
var
  Str: string;
  i: Integer;
begin
  Str := '';
  for i := 0 to Length(Dirs)-1 do
    Str += 'r"' + Dirs[i] + '"' + ',';
  Str += '""';
  Str := Format('sys.path = [%s]', [Str]);
  GetPythonEngine.ExecString(Str);
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  S: string;
begin
  S := {$IFDEF WINDOWS} S_PYTHON_LIBRARY_WINDOWS {$ENDIF}
       {$IFDEF LINUX} S_PYTHON_LIBRARY_LINUX {$ENDIF}
       {$IFDEF DARWIN} S_PYTHON_LIBRARY_MACOS {$ENDIF};
  PythonEngine1.DllPath := ExtractFileDir(S);
  PythonEngine1.DllName := ExtractFileName(S);
  PythonEngine1.LoadDll;
end;


procedure TForm1.PythonEngine1AfterInit(Sender: TObject);
var
  ADir: string;
begin
{$IFDEF WINDOWS}
  //ADir:= ExtractFilePath(Application.ExeName);
  Py_SetSysPath([
    {ADir + }'D:\Program Files\Python312\DLLs\',
    {ADir + }'D:\Program Files\Python312\Lib\'
  ]);
{$ENDIF}
end;

procedure TForm1.PythonInputOutput1SendData(Sender: TObject;
  const Data: AnsiString);
begin
  memoConsole.Lines.Add(Data);
end;

procedure TForm1.PythonInputOutput1SendUniData(Sender: TObject;
  const Data: UnicodeString);
begin
  memoConsole.Lines.Add(Data);
end;

procedure TForm1.edConsoleKeyPress(Sender: TObject; var Key: char);
var
  Str: string;
begin
  case Ord(Key) of
    VK_RETURN:  begin
                  Str := edConsole.Text;

                  //support entering "=some cmd"
                  if (Str <> '') and (Str[1] = '=') then
                    Str:= 'print('+Copy(Str, 2, MaxInt) + ')';

                  memoConsole.Lines.Add('>>> ' + Str);
                  edConsole.Text := '';
                  edConsole.Items.Insert(0, Str);
                  try
                    GetPythonEngine.ExecString(Str);
                  except
                  end;
                end;
  end;
end;

end.

