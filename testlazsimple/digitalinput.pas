{:
  @abstract(Модуль окна ввода пароля пользователя)
  @author(Леви Вячеслав Валерьевич)
  @created(200x)
  @lastmod(22.03.2024)

  ********************************* History ************************************
  ******************************************************************************
  22.03.2024 Конвертировал в Lazarus.
             Переименовал модуль из digitalinput в digitalinput.
  @author(Andry Smirnov <Andry.Smirnov@gmail.com>)
  ******************************************************************************
  30.11.2022 Изменил свойства формы.
  @author(Andry Smirnov <Andry.Smirnov@gmail.com>)
  ******************************************************************************
  31.10.2022 Добавил в начало файла строку кодировки %encoding CP1251.
  @author(Andry Smirnov <Andry.Smirnov@gmail.com>)
  ******************************************************************************
  26.10.2022 Добавил строку включения режима FPC DELPHI.
             Добавил строку выбора загружаемой формы.
             Список модулей выстроил в линию.
             Объявление модуля Variants для разных Delphi различается.
  @author(Andry Smirnov <Andry.Smirnov@gmail.com>)
  ******************************************************************************
}
unit digitalinput;


{$mode ObjFPC}{$H+}
{ $I scadaeisa.inc}
{$IFDEF ShowCompiledUnitCaption}
  {$HINT Compile digitalinput}
{$ENDIF}


interface


uses
  LCLIntf,
  LCLType,
  //LMessages,
  Variants,
  SysUtils,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  //
  sybutton
  ;


type
  TDigitalInputForm = class( TForm )
    btn0: TSYButton;
    btn1: TSYButton;
    btn2: TSYButton;
    btn3: TSYButton;
    btn4: TSYButton;
    btn5: TSYButton;
    btn6: TSYButton;
    btn7: TSYButton;
    btn8: TSYButton;
    btn9: TSYButton;
    btnBackspace: TSYButton;
    btnClear: TSYButton;
    btnDecimal: TSYButton;
    btnCancel: TSYButton;
    btnEnter: TSYButton;
    btnMinus: TSYButton;
    edtPassword: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btn0Click( Sender: TObject );
    procedure btn0PressDown( Sender: TObject );
    procedure btnBackspaceClick( Sender: TObject );
    procedure btnClearClick( Sender: TObject );
    procedure btnDecimalClick( Sender: TObject );
    procedure btnEnterClick( Sender: TObject );
    procedure btnMinusClick( Sender: TObject );
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure FormKeyPress( Sender: TObject; var Key: Char );
    procedure FormShow( Sender: TObject );
  public
    // Режим ввода пароля
    PasswordMode: Boolean;
    // Число - результат
    ResultReal: Real;
  end;


var
  DigitalInputForm: TDigitalInputForm;


implementation


(*
uses
  eisamain,
  eisatypes
  ;
*)


{$R *.lfm}


const
  S_INPUT_PASSWORD = 'Введите пароль';
  S_INPUT_NUMBERS = 'Цифровой ввод';


procedure TDigitalInputForm.btn0Click( Sender: TObject );
begin
  // Нажатие цифры
  if ( ( edtPassword.Text <> '0' ) or
    ( ( Sender as TSYButton ).Caption <> '0' ) ) and
    ( Length( edtPassword.Text ) < 9 ) {//Max количество знаков} then
    edtPassword.Text := edtPassword.Text + ( Sender as TSYButton ).Caption;
end;


procedure TDigitalInputForm.btn0PressDown( Sender: TObject );
begin
  // Звук нажатия
  //MainForm.PlaySound( SOUND_MENU );
end;


procedure TDigitalInputForm.btnBackspaceClick( Sender: TObject );
begin
  // Удаление последнего символа
  if edtPassword.Text <> '' then
    edtPassword.Text := Copy( edtPassword.Text, 1, Length( edtPassword.Text ) - 1 );
end;


procedure TDigitalInputForm.btnClearClick( Sender: TObject );
begin
  // Очистка поля ввода
  edtPassword.Text := '';
end;


procedure TDigitalInputForm.btnDecimalClick( Sender: TObject );
begin
  // Нажатие ","
  if Pos( ',', edtPassword.Text ) = 0 then
    edtPassword.Text := edtPassword.Text + ',';
  if edtPassword.Text = ',' then
    edtPassword.Text := '0,';
  if edtPassword.Text = '-,' then
    edtPassword.Text := '-0,';
end;


procedure TDigitalInputForm.btnEnterClick( Sender: TObject );
begin
  // Ввод
  if edtPassword.Text <> '' then
    try
      ResultReal := StrToFloat( edtPassword.Text );
    except
      ResultReal := 0;
    end;
end;


procedure TDigitalInputForm.btnMinusClick( Sender: TObject );
begin
  // Нажатие "-"
  if edtPassword.Text = '' then
    edtPassword.Text := '-'
  else
    if edtPassword.Text = '-' then
      edtPassword.Text := ''
    else
      if Copy( edtPassword.Text, 1, 1 ) = '-' then
        edtPassword.Text := Copy( edtPassword.Text, 2, Length( edtPassword.Text ) )
      else
        edtPassword.Text := '-' + edtPassword.Text;
end;


procedure TDigitalInputForm.FormDeactivate(Sender: TObject);
begin
  Close;
end;


procedure TDigitalInputForm.FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
begin
  case Key of
    VK_0,
    VK_NUMPAD0: btn0Click( btn0 );
    VK_1,
    VK_NUMPAD1: btn0Click( btn1 );
    VK_2,
    VK_NUMPAD2: btn0Click( btn2 );
    VK_3,
    VK_NUMPAD3: btn0Click( btn3 );
    VK_4,
    VK_NUMPAD4: btn0Click( btn4 );
    VK_5,
    VK_NUMPAD5: btn0Click( btn5 );
    VK_6,
    VK_NUMPAD6: btn0Click( btn6 );
    VK_7,
    VK_NUMPAD7: btn0Click( btn7 );
    VK_8,
    VK_NUMPAD8: btn0Click( btn8 );
    VK_9,
    VK_NUMPAD9: btn0Click( btn9 );
    VK_DECIMAL: btn0Click( btnDecimal );
    VK_BACK:    btnBackspaceClick( btnBackspace );
    VK_RETURN:  btnEnterClick( btnEnter );
    VK_ESCAPE:  ModalResult := mrCancel;
  end;
end;


procedure TDigitalInputForm.FormKeyPress( Sender: TObject; var Key: Char );
begin
  case Ord(Key) of
    VK_RETURN:  begin
                  btnEnterClick( Sender );
                  ModalResult := mrOk;
                end;
    VK_ESCAPE:  ModalResult := mrCancel;
  end;
end;


procedure TDigitalInputForm.FormShow( Sender: TObject );
begin
  edtPassword.Text := '';
  //edtPassword.PasswordChar := '•';
  ResultReal := 0;
  if PasswordMode then
    begin
      Caption := S_INPUT_PASSWORD;
      btnDecimal.Enabled := False;
      btnMinus.Enabled := False;
      edtPassword.PasswordChar := '*';//#149;
    end
  else
    begin
      Caption := S_INPUT_NUMBERS;
      btnDecimal.Enabled := True;
      btnMinus.Enabled := True;
      edtPassword.PasswordChar := #0;
    end
end;


{--- Initialization and Finalization ---}


initialization
  RegisterClass( TDigitalInputForm );


end.

