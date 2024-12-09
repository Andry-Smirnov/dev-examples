{
Stretch the image
-----------------

To stretch the image, we need to create a temporary stretched image:

```pascal
procedure TForm1.FormPaint(Sender: TObject);
var stretched: TBGRABitmap;
begin
  stretched := image.Resample(ClientWidth, ClientHeight) as TBGRABitmap;
  stretched.Draw(Canvas,0,0,True);
  stretched.Free;
end;
```

By default, it uses fine resample, but you can precise if you want to use simple stretch instead (faster):

```pascal
stretched := image.Resample(ClientWidth, ClientHeight, rmSimpleStretch) as TBGRABitmap;
```

You can also specify the interpolation filter with the ResampleFilter property:

```pascal
image.ResampleFilter := rfMitchell;
stretched := image.Resample(ClientWidth, ClientHeight) as TBGRABitmap;
```
}
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
  ExtCtrls,
  //
  //BGRALayers,
  BGRABitmap,
  BGRASVG,
  BGRAUnits,
  BGRABitmapTypes,
  BCSVGViewer;


type

  { TMainForm }

  TMainForm = class(TForm)
    BCSVGViewer1: TBCSVGViewer;
    LoadImageButton: TButton;
    ExitButton: TButton;
    PaintImage: TImage;
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LoadImageButtonClick(Sender: TObject);
  private
    FMnemoImage: TBGRABitmap;
    FSize: Single;

    procedure DrawBrush(X, Y: Integer);
    // Выводим фиолетовый круг поверх мнемосхемы
    procedure PaintNoLink;
    // Выводим голубой прямоугольник поверх нарисованного
    procedure PaintSelection;
    procedure PaintMnemo;
    // Перерисовываем мнемосхему на экран
    procedure OutMnemo;
  public
    property MnemoImage: TBGRABitmap read FMnemoImage;
  end;


var
  MainForm: TMainForm;
  // Используемые цвета, задавать как константы не получается
  BGRAEISABackground: TBGRAPixel;
  // Цвет отсутствия связи
  BGRANoLink: TBGRAPixel;
  // Цвет выделения
  BGRASelected: TBGRAPixel;


implementation


{$R *.lfm}


const
  // Цвет фона мнемосхемы
  COLOR_EISA_BACKGROUND = $00848484;


{ TMainForm }

procedure TMainForm.DrawBrush(X, Y: Integer);
const
  RADIUS = 5;
begin
  FMnemoImage.GradientFill(X-RADIUS, Y-RADIUS, X+RADIUS, Y+RADIUS,
    BGRABlack, BGRAPixelTransparent, gtRadial,
    PointF(X, Y), PointF(X+RADIUS, Y), dmDrawWithTransparency);
  OutMnemo;
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  FMnemoImage := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAEISABackground);
  // Располагаем картинку, на которую будем выводить мнемосхему
  PaintImage.Left := 0;
  PaintImage.Top := 0;
  PaintImage.Align := alClient;
  // Необходимо обновить размеры компонентов
  FormResize(Sender);
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FMnemoImage) then
    FreeAndNil(FMnemoImage);
end;


procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    DrawBrush(X,Y);
end;


procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
    DrawBrush(X,Y);
end;


procedure TMainForm.FormPaint(Sender: TObject);
begin
  OutMnemo;
end;


procedure TMainForm.FormResize(Sender: TObject);
begin
  FMnemoImage.SetSize(ClientWidth, ClientHeight);
  // Изменяем размер картинки в которой рисуем
  PaintImage.Picture.Bitmap.Height := FMnemoImage.Height;
  PaintImage.Picture.Bitmap.Width := FMnemoImage.Width;
  if FMnemoImage.Width > FMnemoImage.Height then
    FSize := FMnemoImage.Height / 2
  else
    FSize := FMnemoImage.Width / 2;
  PaintMnemo;
  OutMnemo;
end;


procedure TMainForm.LoadImageButtonClick(Sender: TObject);
var
  BmpImage: TBGRABitmap;
  SvgImage: TBGRASVG;
begin
  //PaintMnemo;
  BmpImage := TBGRABitmap.Create;
  try
    //Linear Fill 1.svg
    //tiger.svg
    //linecap.svg
    SvgImage := TBGRASVG.Create('../../../image/svg/Linear Fill 1.svg');//100, 100, TCSSUnit.cuPercent);
    try
      //SvgImage.LoadFromFile('linecap.svg');
      BmpImage.SetSize(Round(FMnemoImage.Width), Round(FMnemoImage.Height));
      //SvgImage.Draw(BmpImage.Canvas2D, 0, 0, cuPixel);
      SvgImage.StretchDraw(BmpImage.Canvas2D, 0, 0, FMnemoImage.Width, FMnemoImage.Height);

      //SvgImage.Draw(FMnemoImage.Canvas2D, 0, 0, cuPixel);
      //SvgImage.StretchDraw(FMnemoImage.Canvas2D, 0, 0, FMnemoImage.Width, FMnemoImage.Height, True);
    finally
      SvgImage.Free;
    end;
    //Image1.Picture.Bitmap.Assign(BmpImage);
    PaintImage.Picture.Bitmap.Assign(BmpImage);
    FMnemoImage.PutImage(0, 0, BmpImage, dmDrawWithTransparency);
  finally
    BmpImage.Free;
  end;
  OutMnemo;
end;


procedure TMainForm.OutMnemo;
begin
  FMnemoImage.Draw(PaintImage.Canvas, 0, 0);
end;


procedure TMainForm.PaintMnemo;
// Рисуем мнемосхему
begin
  FMnemoImage.Fill(COLOR_EISA_BACKGROUND, dmSet);
  //
  PaintNoLink;
  PaintSelection;
end;


procedure TMainForm.PaintNoLink;
// Выводим фиолетовый круг поверх мнемосхемы
var
  NoLinkImage: TBGRABitmap;
begin
  NoLinkImage := TBGRABitmap.Create(FMnemoImage.Width, FMnemoImage.Height);
  try
    NoLinkImage.FillEllipseAntialias(FSize, FSize, FSize, FSize, BGRANoLink);
    FMnemoImage.PutImage(0, 0, NoLinkImage, dmDrawWithTransparency);
  finally
    NoLinkImage.Free;
  end;
end;


procedure TMainForm.PaintSelection;
// Выводим голубой прямоугольник поверх нарисованного
const
  SELECTED_SIZE = 20 /2;
var
  SelectedImage: TBGRABitmap;
begin
  // Выводим фиолетовый круг поверх мнемосхемы
  SelectedImage := TBGRABitmap.Create(FMnemoImage.Width, FMnemoImage.Height);
  try
    SelectedImage.FillRectAntialias(0, FSize/2-SELECTED_SIZE,
      FSize*2, FSize/2+SELECTED_SIZE, BGRASelected);
    FMnemoImage.PutImage(0, 0, SelectedImage, dmDrawWithTransparency);
  finally
    SelectedImage.Free;
  end;
end;


initialization
  // Используемые цвета, задавать как константы не получается
  BGRAEISABackground := BGRA($84, $84, $84, $FF);
  BGRANoLink := BGRA($E0, $00, $E0, $40);
  BGRASelected := BGRA($00, $E0, $E0, $40);


end.

