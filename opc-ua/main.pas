{$CODEPAGE UTF8}
unit main;


{$mode objfpc}{$H+}


interface


uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ComCtrls,
  //
  AnchorDockPanel,
  //
  open62541
  ;


type

  { TMainForm }

  TMainForm = class(TForm)
    AnchorDockPanel1: TAnchorDockPanel;
    LoadLibraryButton: TButton;
    StatusBar1: TStatusBar;
    UnloadLibraryButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LoadLibraryButtonClick(Sender: TObject);
    procedure UnloadLibraryButtonClick(Sender: TObject);
  private
  public
  end;


var
  MainForm: TMainForm;


implementation


{$R *.lfm}


const
  S_LIB_CONNECTED = 'Загружена';
  S_LIB_DISCONNECTED = 'Отключена';


procedure TMainForm.LoadLibraryButtonClick(Sender: TObject);
begin
  LoadOpen62541;
  if LibLoaded then
    StatusBar1.Panels[0].Text := S_LIB_CONNECTED;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := S_LIB_DISCONNECTED;
end;


procedure TMainForm.UnloadLibraryButtonClick(Sender: TObject);
begin
  UnloadOpen62541;
  StatusBar1.Panels[0].Text := S_LIB_DISCONNECTED;
end;


end.

