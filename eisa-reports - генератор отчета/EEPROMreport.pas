{
  Модуль главного окна SCADA EISA @br
  @br
  Зависимости:
    @link( Windows), @link( Classes ), @link( Forms ),
    @link( Graphics ), @link( SysUtils ), @link( Dialogs),
    @link( Controls ), @link( Messages ), @link( Variants ),
    @link( frxClass ) @br

  @author Andry Smirnov

  @created 2018.02
  @lastmod 2018–03–12

  Помощь: https://www.fastreport.ru/ru/forum/index.php?showtopic=3518
}
//DONE 5 -oAndry -cReg Components : Необходимо исправить компоненты настроек — компоненты должны иметь общего предка, с общими полями, например, значение, Датаплейс, тип и т.п.
unit EEPROMreport;

{$I reports-config.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  ComCtrls,
  Forms,
  frxClass;

type
  { @abstract Настройка ЕЕПРОМ }
  { @member Name Название настройки }
  { @member Value Значение }
  TValueRec = record
    Name: string;
    Value: string;
  end;

  { @abstract Настройки в группе }
  TValues = array of TValueRec;

  { @abstract Группа настроек }
  { @member Name Название группы }
  { @member Values Настройки группы }
  TGroupRec = record
    Name: string;
    Values: TValues;
    ValuesCount: Integer;
  end;

  { @abstract Все группы настроек }
  TGroups = array of TGroupRec;

  { @abstract Форма отчёта настроек }
  TfrmEEPROMreport = class(TForm)
    ReportEEPROM: TfrxReport;
    udsValues: TfrxUserDataSet;
    procedure FormDestroy(Sender: TObject);
    procedure udsValuesCheckEOF(Sender: TObject; var Eof: Boolean);
    procedure udsValuesFirst(Sender: TObject);
    procedure udsValuesNext(Sender: TObject);
    procedure udsValuesGetValue(const VarName: string; var Value: Variant);
  private
    // Все настройки
    FGroups: TGroups;
    FGroup: Integer;
    FValue: Integer;
    procedure FreeMemory;
    function GetGroupsCount: Integer;
    function GetValuesCount: Integer;
  public
    procedure ShowReport(Form: TForm);

    // Текущее положение в группах
    //property Group: Integer read FGroup write FGroup default 0;
    property GroupsCount: Integer read GetGroupsCount;
    // Текущее положение в настройках текущей группы
    //property Value: Integer read FValue write FValue default 0;
    property ValuesCount: Integer read GetValuesCount;
  end;

var
  frmEEPROMreport: TfrmEEPROMreport;

function ParseGroups(Form: TForm): TGroups;
procedure FreeGroups(Groups: TGroups);

implementation

uses
  (*
    YSBitParamReg,
    YSSmallParamReg,
    YSParamReg,
    YSRealParamReg,
  *)
  RzTabs,
  SLAParamReg,
  SLAParamRegByte,
  SLAParamRegNum,
  SLAParamRegOpt,
  SLAParamRegScrl,
  SLAParamRegTemp;

{$R *.dfm}

{ TfrmEEPROMreport }

function ParseGroups(Form: TForm): TGroups;
var
  i: Integer;
  j: Integer;
  k: Integer;
  l: Integer;
  wc1: TWinControl;
  wc2: TWinControl;
  pr: TSLAParamReg;
  Val: TValues;
begin
  if not Assigned(Form) and (Form.ComponentCount = 0) then
    begin
      Exit;
      //TODO 2 -oAndry -cAdd log : Добавить сообщение о отсутствии настроек на форме
      //AddToLog();
    end;
  // Предварительно очищаем заполненые настройки
  FreeGroups(Result);
  // Заполняем отчёт группами настроек
  for l := 0 to Form.ControlCount - 1 do
    if {$IFDEF USE_RC}(Form.Controls[l] is TRzPageControl) or {$ENDIF}
      (Form.Controls[l] is TPageControl) then
      begin
        wc1 := TWinControl(Form.Controls[l]);
        for i := 0 to wc1.ControlCount - 1 do //pc.ComponentCount - 1 do
          if {$IFDEF USE_RC}(wc1.Controls[i] is TRzTabSheet) or {$ENDIF}
            (wc1.Controls[i] is TTabSheet) then
            begin
              wc2 := TWinControl(wc1.Controls[i]);
              // Если на странице нет компонентов, переходим к следующей странице
              if wc2.ControlCount = 0 then
                Continue;
              SetLength(Val, 0);
              // Парсим все настройки на странице
              for j := 0 to wc2.ControlCount - 1 do
                if wc2.Controls[j] is TSLAParamReg then
                  begin
                    pr := TSLAParamReg(wc2.Controls[j]);
                    k := Length(Val);
                    SetLength(Val, k + 1);
                    // Заполняем название настройки
                    Val[k].Name := pr.DescriptionText;
{$IFDEF DEBUG}
                    if Length(Val[k].Name) = 2 then
                      Val[k].Name := 'Пусто';
{$ENDIF}
                    // Получаем значение
                    Val[k].Value := pr.FlashValueStr;
                  end;
              // Если настроек не нашли, то группу в отчёт не добавляем
              if Length(Val) <> 0 then
                begin
                  // Текущий индекс
                  k := Length(Result);
                  //Добавляем элемент
                  SetLength(Result, k + 1);
                  // Название группы
{$IFDEF USE_RC}
                  if wc1.Controls[i] is TRzTabSheet then
                    Result[k].Name := TRzTabSheet(wc2).Caption;
{$ENDIF}
                  if wc1.Controls[i] is TTabSheet then
                    Result[k].Name := TTabSheet(wc2).Caption;
                  // Копируем заполненые настройки
                  Result[k].Values := Copy(Val, 0, Length(Val));
                  // Заполняем поле количества записей настроек
                  Result[k].ValuesCount := Length(Val);
                end;
            end;
      end;
end;

procedure FreeGroups(Groups: TGroups);
var
  i: Integer;
begin
  // Освобождение памяти из-под динамического массива
  if Length(Groups) > 0 then
    for i := 0 to Length(Groups) - 1 do
      if Length(Groups[i].Values) > 0 then
        SetLength(Groups[i].Values, 0);
  SetLength(Groups, 0);
end;

procedure TfrmEEPROMreport.ShowReport(Form: TForm);
begin
  // Предварительно очищаем заполненые настройки
  FreeMemory;
  // Парсим настройки
  FGroups := ParseGroups(Form);
  // Выводим отчёт
  ReportEEPROM.ShowReport;
end;

procedure TfrmEEPROMreport.FormDestroy(Sender: TObject);
begin
  FreeMemory;
end;

procedure TfrmEEPROMreport.FreeMemory;
begin
  FreeGroups(FGroups);
end;

function TfrmEEPROMreport.GetGroupsCount: Integer;
begin
  Result := Length(FGroups);
end;

function TfrmEEPROMreport.GetValuesCount: Integer;
begin
  if GroupsCount > 0 then
    Result := FGroups[FGroup].ValuesCount
  else
    Result := 0;
end;

procedure TfrmEEPROMreport.udsValuesFirst(Sender: TObject);
begin
  FGroup := 0;
  FValue := 0;
end;

procedure TfrmEEPROMreport.udsValuesNext(Sender: TObject);
begin
  Inc(FValue);
  if FValue >= ValuesCount then
    begin
      FValue := -1;
      Inc(FGroup);
    end;
end;

procedure TfrmEEPROMreport.udsValuesCheckEOF(Sender: TObject; var Eof: Boolean);
begin
  Eof := (GroupsCount = 0) or (FGroup >= GroupsCount);
end;

procedure TfrmEEPROMreport.udsValuesGetValue(const VarName: string; var Value: Variant);
begin
  if CompareText(VarName, 'index') = 0 then
    Value := FGroup + 1;
  if CompareText(VarName, 'groupName') = 0 then
    if FGroup < GroupsCount then
      Value := FGroups[FGroup].Name;
  if CompareText(VarName, 'valueName') = 0 then
    if FGroup < GroupsCount then
      if FValue < ValuesCount then
        Value := FGroups[FGroup].Values[FValue].Name;
  if CompareText(VarName, 'value') = 0 then
    if FGroup < GroupsCount then
      if FValue < ValuesCount then
        Value := FGroups[FGroup].Values[FValue].Value;
end;

end.

