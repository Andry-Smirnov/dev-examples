object frmMain: TfrmMain
  Left = 722
  Top = 314
  BorderStyle = bsSingle
  Caption = #1054#1090#1095#1105#1090#1099
  ClientHeight = 54
  ClientWidth = 284
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  DesignSize = (
    284
    54)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExit: TButton
    Left = 192
    Top = 13
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Exit'
    Default = True
    TabOrder = 0
    OnClick = btnExitClick
  end
  object btnReport: TButton
    Left = 104
    Top = 13
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Report!'
    TabOrder = 1
    OnClick = btnReportClick
  end
  object btn1: TButton
    Left = 16
    Top = 13
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Show form'
    TabOrder = 2
    OnClick = btn1Click
  end
end
