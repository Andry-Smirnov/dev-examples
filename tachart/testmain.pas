unit testmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls,
  //Math,
  TAGraph, TAFuncSeries, TASeries,
  TASources, TAGUIConnectorBGRA,
  TACustomSource;

type

  { TMainForm }

  TMainForm = class(TForm)
    FillRandomButton: TButton;
    CloseButton: TButton;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    Graph1CheckBox: TCheckBox;
    Graph2CheckBox: TCheckBox;
    Graph3CheckBox: TCheckBox;
    Graph4CheckBox: TCheckBox;
    TrendChart: TChart;
    TrendChartBSplineSeries1: TBSplineSeries;
    TrendChartFitSeries1: TFitSeries;
    GraphAverageSeries: TLineSeries;
    GraphSeries: TLineSeries;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    UserDefinedChartSource1: TUserDefinedChartSource;
    procedure FillRandomButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Graph1CheckBoxChange(Sender: TObject);
    procedure Graph2CheckBoxChange(Sender: TObject);
    procedure Graph3CheckBoxChange(Sender: TObject);
    procedure Graph4CheckBoxChange(Sender: TObject);
  private
    procedure UserDefinedChartSource1GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);

  public

  end;


var
  MainForm: TMainForm;


implementation


{$R *.lfm}


const
  POINTS_COUNT = 100;
  MAX_VALUE = 100;
  MIDDLE_POINT = 0;


var
  PointsArray: array of Double;


procedure TMainForm.FillRandomButtonClick(Sender: TObject);
var
  I: Integer;
begin
  GraphSeries.Clear;
  for I := 0 to POINTS_COUNT do
    GraphSeries.AddXY( I, (MIDDLE_POINT + MAX_VALUE div 2) - Random(MAX_VALUE) );
end;


procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;


procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  inherited;
  Randomize;
  SetLength(PointsArray, POINTS_COUNT);
  for I := 0 to POINTS_COUNT do
    PointsArray[I] := (MIDDLE_POINT + MAX_VALUE div 2) - Random(MAX_VALUE);
  UserDefinedChartSource1.OnGetChartDataItem := @UserDefinedChartSource1GetChartDataItem;
end;


procedure TMainForm.Graph1CheckBoxChange(Sender: TObject);
begin
  GraphSeries.Active := Graph1CheckBox.Checked;
end;


procedure TMainForm.Graph2CheckBoxChange(Sender: TObject);
begin
  GraphAverageSeries.Active := Graph2CheckBox.Checked;
end;


procedure TMainForm.Graph3CheckBoxChange(Sender: TObject);
begin
  TrendChartBSplineSeries1.Active := Graph3CheckBox.Checked;
end;


procedure TMainForm.Graph4CheckBoxChange(Sender: TObject);
begin
  TrendChartFitSeries1.Active := Graph4CheckBox.Checked;
end;


procedure TMainForm.UserDefinedChartSource1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := now + ( AIndex / (24*60*60) );
  if Assigned(PointsArray) then
    AItem.Y := PointsArray[AIndex]
  else
    AItem.Y := (MIDDLE_POINT + MAX_VALUE div 2) - Random(MAX_VALUE);
end;


end.

