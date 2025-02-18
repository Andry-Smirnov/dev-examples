unit FormMain;

{$mode objfpc}{$H+}
{$Codepage utf8}

interface


uses
  LclType,
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Menus,
  StdCtrls,
  PythonEngine,
  PythonGUIInputOutput
  ;


type
  { TfmMain }
  TfmMain = class(TForm)
    edConsole: TEdit;
    memoConsole: TMemo;
    mnuHelpAbout: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    PythonEngine: TPythonEngine;
    PythonInputOutput1: TPythonInputOutput;
    PythonModule1: TPythonModule;
    procedure edConsoleKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memoConsoleDblClick(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure PythonEngineAfterInit(Sender: TObject);
    procedure PythonInputOutput1SendData(Sender: TObject; const Data: AnsiString);
    procedure PythonInputOutput1SendUniData(Sender: TObject; const Data: UnicodeString);
    procedure PythonModule1Initialization(Sender: TObject);
  private
    FList: TStringList;

    procedure InitPythonEngine;
    procedure DoLogConsoleLine(const Str: string);
    procedure DoExecuteConsoleLine(Str: string);
  public
  end;


var
  fmMain: TfmMain;


implementation


{$R *.lfm}


const
  S_PYTHON_VERSION_MAJOR = '3';
{$IFDEF WINDOWS}
  S_PYTHON_LIB_WINDOWS: string = 'python' + S_PYTHON_VERSION_MAJOR + '12.dll';
  S_PYTHON_ZIP_WINDOWS: string = 'python312.zip';
{$ENDIF}
{$IFDEF LINUX}
  S_PYTHON_LIB_LINUX: string = 'libpython3.8.so.1.0'; //default in Ubuntu 20.x
{$ENDIF}
{$IFDEF DARWIN}
  S_PYTHON_LIB_MACOS: string = '/Library/Frameworks/Python.framework/Versions/3.7/lib/libpython3.7.dylib';
{$ENDIF}

  // Фиксированные шрифты
{$IFDEF WINDOWS}
  FONT_FIXED_NAME = 'Iosevka';
{$ENDIF}
{$IFDEF LINUX}
  FONT_FIXED_NAME = 'Ubuntu Mono';
{$ENDIF}
{$IFDEF DARWIN}
  FONT_FIXED_NAME = 'Monaco';
{$ENDIF}
{$IFDEF WINDOWS}
  FONT_FIXED_SIZE = 9;
{$ELSE}
  FONT_FIXED_SIZE = 11;
{$ENDIF}

// Обычные шрифты
{$IFDEF WINDOWS}
  FONT_VAR_NAME = 'default';
{$ENDIF}
{$IFDEF LINUX}
  FONT_VAR_NAME = 'Ubuntu';
{$ENDIF}
{$IFDEF DARWIN}
  FONT_VAR_NAME = 'default';
{$ENDIF}
{$IFDEF WINDOWS}
  FONT_VAR_SIZE = 9;
{$ELSE}
  FONT_VAR_SIZE = 10;
{$ENDIF}

  // Максимальное количество команд в списке
  MAX_LINES = 1000;
  // Префикс строки команды Пайтона
  S_PYTHON_PROMPT = '>>> ';


procedure SetPythonSysPath(const Dirs: array of string; DoAdd: Boolean);
var
  Str: string;
  Sign: string;
  i: Integer;
begin
  Str := '';
  for i := 0 to Length(Dirs) - 1 do
    Str := Str + 'r"' + Dirs[i] + '"' + ', ';
  if DoAdd then
    Sign := '+='
  else
    Sign := '=';
  Str := Format('sys.path %s [%s]', [Sign, Str]);
  GetPythonEngine.ExecString(Str);
end;


function Py_s0(Self, Args : PPyObject): PPyObject; cdecl;
begin
  with GetPythonEngine do
    Result := PyUnicode_FromString('1.0.0');
end;


function Py_s1(Self, Args : PPyObject): PPyObject; cdecl;
const
  S0: string = 'begin.Привет.end';
begin
  with GetPythonEngine do
    Result := PyUnicode_FromString(PChar(S0));
end;


function Py_n1(Self, Args : PPyObject): PPyObject; cdecl;
begin
  with GetPythonEngine do
    Result := PyLong_FromLong(-100000);
end;


{ TfmMain ---------------------------------------------------------------------}


procedure TfmMain.DoLogConsoleLine(const Str: string);
begin
  with memoConsole do
    begin
      Lines.BeginUpdate;
      while Lines.Count > MAX_LINES do
        Lines.Delete(0);
      Lines.Add(Str);
      Lines.EndUpdate;

      SelStart:= Length(Lines.Text) - 1;
    end;
end;


procedure TfmMain.DoExecuteConsoleLine(Str: string);
var
  i: Integer;
begin
  DoLogConsoleLine(S_PYTHON_PROMPT + Str);
  edConsole.Text := '';

  i := FList.IndexOf(Str);
  if i >= 0 then
    FList.Delete(i);
  FList.Insert(0, Str);

  if (Str <> '') and (Str[1] = '=') then
    Str := 'print('+Copy(Str, 2, MaxInt) + ')';

  try
    GetPythonEngine.ExecString(Str);
  except
  end;
end;


procedure TfmMain.PythonInputOutput1SendData(Sender: TObject;
  const Data: AnsiString);
begin
  DoLogConsoleLine(Data);
end;


procedure TfmMain.PythonInputOutput1SendUniData(Sender: TObject;
  const Data: UnicodeString);
begin
  DoLogConsoleLine(Data);
end;


procedure TfmMain.PythonModule1Initialization(Sender: TObject);
begin
  with Sender as TPythonModule do
    begin
      AddMethod('s0', @Py_s0, '');
      AddMethod('s1', @Py_s1, '');
      AddMethod('n1', @Py_n1, '');
    end;
end;


procedure TfmMain.FormShow(Sender: TObject);
begin
  InitPythonEngine;
end;


procedure TfmMain.memoConsoleDblClick(Sender: TObject);
var
  N: Integer;
  Str: string;
begin
  with memoConsole do
    begin
      N := CaretPos.Y;
      if (N >= 0) and (N < Lines.Count) then
        begin
          Str := Lines[N];
          if Copy(Str, 1, Length(S_PYTHON_PROMPT)) = S_PYTHON_PROMPT then
            begin
              Delete(Str, 1, Length(S_PYTHON_PROMPT));
              DoExecuteConsoleLine(Str);
            end;
        end;
    end;
end;

procedure TfmMain.PopupMenu1Popup(Sender: TObject);
var
  i: Integer;
  mi: TMenuItem;
begin
  with PopupMenu1 do
    begin
      Items.Clear;
      for i := 0 to FList.Count - 1 do
        begin
          mi := TMenuItem.Create(Self);
          mi.Caption := FList[i];
          mi.Tag := i;
          mi.OnClick := @MenuClick;
          Items.Add(mi);
        end;
    end;
end;


procedure TfmMain.MenuClick(Sender: TObject);
var
  N: Integer;
  Str: string;
begin
  N := (Sender as TMenuItem).Tag;
  if N < FList.Count then
    begin
      Str := FList[N];
      DoExecuteConsoleLine(Str);
    end;
end;


procedure TfmMain.edConsoleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  P: TPoint;
begin
  case Key of
    VK_RETURN:  begin
                  DoExecuteConsoleLine(edConsole.Text);
                  Key:= 0;
                end;
    VK_DOWN,
    VK_UP:      begin
                  P := edConsole.ClientToScreen( Point(0, 0) );
                  PopupMenu1.Popup(P.X, P.Y);
                  Key := 0;
                end;
  end;
end;


procedure TfmMain.FormCreate(Sender: TObject);
begin
  FList := TStringList.Create;
end;


procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FList);
end;


procedure TfmMain.PythonEngineAfterInit(Sender: TObject);
//const
  //NTest: Longint = 1 shl 30;
var
  Dir: string;
begin
  Dir := ExtractFilePath(Application.ExeName);
{$IFDEF WINDOWS}
  SetPythonSysPath([Dir + 'DLLs', Dir + S_PYTHON_ZIP_WINDOWS], False);
{$ENDIF}
  SetPythonSysPath([Dir + 'Py'], True);

  //test for LongInt
  //Caption := BoolToStr(PythonEngine.PyInt_AsLong(PythonEngine.PyInt_FromLong(NTest)) = NTest, True);
end;


procedure TfmMain.InitPythonEngine;
var
  Str: string;
begin
  Str :=  {$IFDEF WINDOWS} S_PYTHON_LIB_WINDOWS {$ENDIF}
          {$IFDEF LINUX} S_PYTHON_LIB_LINUX {$ENDIF}
          {$IFDEF DARWIN} S_PYTHON_LIB_MACOS {$ENDIF};
  PythonEngine.DllPath := ExtractFileDir(Str);
  PythonEngine.DllName := ExtractFileName(Str);
  PythonEngine.LoadDll;
end;


{$IFDEF DARWIN}
procedure InitMacLibPath;
var
  N: Integer;
  Str: string;
begin
  for N := 11 downto 5 do
    begin
      Str := Format('/Library/Frameworks/Python.framework/Versions/3.%d/lib/libpython3.%d.dylib', [N, N]);
      if FileExists(Str) then
        begin
          cPyLibraryMac := Str;
          Exit;
        end;
    end;
end;
{$ENDIF}


initialization
{$IFDEF DARWIN}
  InitMacLibPath;
{$ENDIF}


end.

