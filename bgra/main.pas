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
  //
  BGRABitmap,
  BGRALayers,
  BGRABitmapTypes;


type

  { TMainForm }

  TMainForm = class(TForm)
    ExitButton: TButton;
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure DrawBrush(X, Y: Integer);
    procedure PaintImage;
  public
    FMnemoImage: TBGRALayeredBitmap;//TBGRABitmap;
  end;


var
  MainForm: TMainForm;


implementation


{$R *.lfm}


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  BackGround: TBGRABitmap;
  ColorBlended: TBGRABitmap;
begin
  inherited;
  FMnemoImage := TBGRALayeredBitmap.Create(ClientWidth, ClientHeight);//COLOR_EISA_BACKGROUND);
  BackGround := TBGRABitmap.Create(FMnemoImage.Width, FMnemoImage.Height);
  BackGround.Fill(clRed, dmSet);//COLOR_EISA_BACKGROUND, dmSet);
  FMnemoImage.AddOwnedLayer(BackGround);
  ColorBlended := TBGRABitmap.Create(FMnemoImage.Width, FMnemoImage.Height);
  ColorBlended.FillEllipseAntialias(FMnemoImage.Width/2, FMnemoImage. Height/2, Height/2, Height/2, BGRA(224, 0, 224, 128));
  FMnemoImage.AddOwnedLayer(ColorBlended);
  FormResize(Sender);
end;


procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
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
  PaintImage;
end;


procedure TMainForm.FormResize(Sender: TObject);
var
  I: Integer;
  //layer: TBGRABitmap;
begin
  FMnemoImage.SetSize(ClientWidth, ClientHeight);
  for I := 0 to FMnemoImage.NbLayers - 1 do
    FMnemoImage.LayerBitmap[I].SetSize(ClientWidth, ClientHeight);
  //FMnemoImage.Fill(clRed, dmSet);//COLOR_EISA_BACKGROUND, dmSet);
  //layer := TBGRABitmap.Create(FMnemoImage.Width, FMnemoImage.Height);
  //layer.FillEllipseAntialias(FMnemoImage.Width/2, FMnemoImage. Height/2, Height/2, Height/2, BGRA(224, 0, 224, 128));
  //layer.EraseEllipseAntialias(layer.Width/2+size*0.15,layer.Height/2,size*0.3,size*0.3,255);
  //FMnemoImage.PutImage(0, 0, layer, dmDrawWithTransparency);
  //layer.Free;
  //FMnemoImage.AddOwnedLayer(layer);
  PaintImage;
end;


procedure TMainForm.DrawBrush(X, Y: Integer);
const
  RADIUS = 5;
begin
(*
  FMnemoImage.GradientFill(X-RADIUS, Y-RADIUS, X+RADIUS, Y+RADIUS,
    BGRABlack, BGRAPixelTransparent, gtRadial,
    PointF(X, Y), PointF(X+RADIUS, Y), dmDrawWithTransparency);
*)
  PaintImage;
end;


procedure TMainForm.PaintImage;
begin
  FMnemoImage.Draw(Canvas, 0, 0);//, True);
end;


end.

