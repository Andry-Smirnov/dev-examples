object frmSHUK_PT_MBParams: TfrmSHUK_PT_MBParams
  Left = 460
  Top = 195
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1082#1086#1090#1083#1072' ('#1064#1059#1050'-'#1055#1058'100)'
  ClientHeight = 757
  ClientWidth = 1060
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 16
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 1060
    Height = 93
    Align = alTop
    BevelOuter = bvNone
    Caption = '  '
    TabOrder = 0
    DesignSize = (
      1060
      93)
    object btnClose: TSYButton
      Left = 961
      Top = 8
      Width = 91
      Height = 48
      Alignment = taCenter
      Anchors = [akTop, akRight]
      Color = 12041670
      BorderColor = 5855577
      Caption = #1047#1072#1082#1088#1099#1090#1100
      Enabled = True
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ButtonStyle = bsRounded
      TextStyle = tsSimple
      TabOrder = 0
      OnClick = btnCloseClick
      ImageIndex = 0
    end
    object NextWindowButton: TSYButton
      Left = 882
      Top = 72
      Width = 179
      Height = 22
      Alignment = taCenter
      Anchors = [akRight, akBottom]
      Color = 6714500
      BorderColor = 2570830
      Caption = #1057#1083#1077#1076#1091#1102#1097#1072#1103' '#1089#1090#1088#1072#1085#1080#1094#1072
      Enabled = True
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ButtonStyle = bsPolygon
      TextStyle = tsRaised
      TabOrder = 1
      ImageIndex = 0
    end
    object RzGroupBox1: TRzGroupBox
      Left = 232
      Top = 6
      Width = 168
      Height = 78
      Caption = 'EEPROM'
      CaptionFont.Charset = RUSSIAN_CHARSET
      CaptionFont.Color = clGrayText
      CaptionFont.Height = -13
      CaptionFont.Name = 'Scada'
      CaptionFont.Style = [fsBold]
      Checked = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -8
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      Transparent = True
      object LabelRd: TLabel
        Left = 12
        Top = 19
        Width = 37
        Height = 13
        Caption = #1063#1090#1077#1085#1080#1077
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 10726063
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object RPercent: TRzProgressStatus
        Left = 54
        Top = 17
        Height = 18
        BorderWidth = 0
        FillColor = 14212578
        FlatColor = 7895160
        ParentFillColor = False
        Transparent = False
        Color = 13489114
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        BarColor = 11910856
        BarColorStop = 13686749
        BarStyle = bsGradient
        GradientDirection = gdHorizontalEnd
        PartsComplete = 0
        Percent = 0
        ShowPercent = True
        TotalParts = 0
      end
      object RGauge: TRzProgressStatus
        Left = 54
        Top = 34
        Height = 8
        BorderWidth = 0
        FillColor = 14212578
        FlatColor = 7895160
        ParentFillColor = False
        Transparent = False
        Color = 13489114
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        BarColor = 16760445
        BarColorStop = 16744448
        BarStyle = bsGradient
        GradientDirection = gdHorizontalEnd
        PartsComplete = 0
        Percent = 0
        TotalParts = 0
      end
      object LabelWr: TLabel
        Left = 14
        Top = 51
        Width = 35
        Height = 13
        Caption = #1047#1072#1087#1080#1089#1100
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 10726063
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object WPercent: TRzProgressStatus
        Left = 54
        Top = 49
        Height = 18
        BorderWidth = 0
        FillColor = 14212578
        FlatColor = 7895160
        ParentFillColor = False
        Transparent = False
        Color = 13489114
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        BarColor = 11910856
        BarColorStop = 13686749
        BarStyle = bsGradient
        GradientDirection = gdHorizontalEnd
        PartsComplete = 0
        Percent = 100
        TotalParts = 0
      end
      object UpdatePanel: TPanel
        Left = 26
        Top = 34
        Width = 8
        Height = 8
        Hint = 
          #1048#1085#1076#1080#1082#1072#1090#1086#1088' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1080' '#1085#1072' '#1092#1086#1088#1084#1077#13'('#1082#1072#1083#1080#1073#1088#1086#1074#1082#1072' '#1087#1088#1077#1076#1077#1083#1086#1074' '#1080' ' +
          #1086#1073#1085#1086#1074#1083#1077#1085#1080#1077' '#1079#1085#1072#1095#1077#1085#1080#1081#13#1087#1072#1088#1072#1084#1077#1090#1088#1086#1074' EEPROM)'
        BevelOuter = bvLowered
        Color = 14146271
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -8
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
    object RzGroupBox2: TRzGroupBox
      Left = 428
      Top = 6
      Width = 331
      Height = 78
      Caption = #1060#1091#1085#1082#1094#1080#1080
      CaptionFont.Charset = RUSSIAN_CHARSET
      CaptionFont.Color = clGrayText
      CaptionFont.Height = -13
      CaptionFont.Name = 'Scada'
      CaptionFont.Style = [fsBold]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -8
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      Transparent = True
      object PrintButton: TSYButton
        Left = 21
        Top = 16
        Width = 86
        Height = 22
        Alignment = taCenter
        Color = 12018282
        BorderColor = 6243664
        Caption = #1055#1077#1095#1072#1090#1100
        Enabled = True
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ButtonStyle = bsRounded
        TextStyle = tsRaised
        TabOrder = 0
        ImageIndex = 0
      end
      object btnWrite: TSYButton
        Left = 21
        Top = 44
        Width = 86
        Height = 22
        Alignment = taCenter
        Color = clRed
        BorderColor = 6243664
        Caption = #1047#1072#1087#1080#1089#1072#1090#1100
        Enabled = True
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ButtonStyle = bsRounded
        TextStyle = tsRaised
        TabOrder = 1
        ImageIndex = 0
      end
      object CopyBuffButton: TSYButton
        Left = 124
        Top = 16
        Width = 86
        Height = 22
        Alignment = taCenter
        Color = 12018282
        BorderColor = 8009791
        Caption = #1042' '#1073#1091#1092#1077#1088
        Enabled = True
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ButtonStyle = bsRounded
        TextStyle = tsRaised
        TabOrder = 2
        ImageIndex = 0
      end
      object PasteBuffButton: TSYButton
        Left = 124
        Top = 44
        Width = 86
        Height = 22
        Alignment = taCenter
        Color = 12018282
        BorderColor = 8009791
        Caption = #1048#1079' '#1073#1091#1092#1077#1088#1072
        Enabled = True
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ButtonStyle = bsRounded
        TextStyle = tsRaised
        TabOrder = 3
        ImageIndex = 0
      end
      object btnSaveParam: TSYButton
        Left = 227
        Top = 16
        Width = 86
        Height = 22
        Alignment = taCenter
        Color = 12029026
        BorderColor = 9201216
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
        Enabled = True
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ButtonStyle = bsRounded
        TextStyle = tsRaised
        TabOrder = 4
        ImageIndex = 0
      end
      object btnLoadParam: TSYButton
        Left = 227
        Top = 44
        Width = 86
        Height = 22
        Alignment = taCenter
        Color = 12029026
        BorderColor = 9201216
        Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' ...'
        Enabled = True
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ButtonStyle = bsRounded
        TextStyle = tsRaised
        TabOrder = 5
        ImageIndex = 0
      end
    end
    object RzGroupBox3: TRzGroupBox
      Left = 15
      Top = 6
      Width = 190
      Height = 78
      Caption = #1064#1082#1072#1092' '#1091#1087#1088#1072#1074#1083#1077#1085#1080#1103
      CaptionFont.Charset = RUSSIAN_CHARSET
      CaptionFont.Color = clGrayText
      CaptionFont.Height = -13
      CaptionFont.Name = 'Scada'
      CaptionFont.Style = [fsBold]
      Checked = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -8
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      Transparent = True
      object TankCB: TComboBox
        Left = 18
        Top = 30
        Width = 157
        Height = 27
        BevelEdges = [beRight, beBottom]
        BevelInner = bvLowered
        BevelKind = bkSoft
        BevelOuter = bvNone
        Style = csDropDownList
        Color = clWhite
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ItemHeight = 19
        ParentFont = False
        TabOrder = 0
      end
    end
    object PrintPanel: TPanel
      Left = 201
      Top = -100
      Width = 540
      Height = 82
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '   '#1055#1088#1086#1090#1086#1082#1086#1083' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1089#1080#1089#1090#1077#1084#1099' '#1085#1072' dd.mm.yyyy  hh:nn'
      Color = 14146271
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -19
      Font.Name = 'Calibri'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      Visible = False
    end
    object CopyPasteGroupBox: TRzGroupBox
      Left = 786
      Top = 6
      Width = 77
      Height = 50
      Caption = #1041#1091#1092#1077#1088
      CaptionFont.Charset = RUSSIAN_CHARSET
      CaptionFont.Color = clGrayText
      CaptionFont.Height = -13
      CaptionFont.Name = 'Scada'
      CaptionFont.Style = [fsBold]
      Color = 14146271
      FlatColor = clYellow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -8
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      Transparent = True
      Visible = False
      object CopyPasteTankLabel: TLabel
        Left = 7
        Top = 22
        Width = 64
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Caption = #1050#1086#1090#1077#1083' 11'
        Color = clBtnFace
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = 16744448
        Font.Height = -13
        Font.Name = 'Calibri'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
    end
  end
  object pcParamWindows: TRzPageControl
    Left = 0
    Top = 93
    Width = 1060
    Height = 664
    ActivePage = TabSheet18
    Align = alClient
    BackgroundColor = clSilver
    BoldCurrentTab = True
    Color = 15790320
    UseColoredTabs = True
    CutCornerSize = 9
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Scada'
    Font.Style = []
    HotTrackColorSource = htcsHotTrackColorProp
    HotTrackStyle = htsTabBar
    MultiLine = True
    ParentBackgroundColor = False
    ParentColor = False
    ParentFont = False
    SortTabMenu = False
    ShowFocusRect = False
    ShowShadow = False
    TabIndex = 0
    TabOrder = 1
    TabOrientation = toBottom
    TabStyle = tsCutCorner
    TextColors.Selected = clBlack
    TextColors.Unselected = clBlack
    FixedDimension = 25
    object TabSheet18: TRzTabSheet
      Color = 15790320
      Caption = ' AST1'#8212'AST6 '
      object Label14: TLabel
        Left = 242
        Top = 58
        Width = 600
        Height = 20
        Caption = 
          #1053#1080#1078#1085#1080#1081' '#1087#1088#1077#1076#1077#1083'                         '#1042#1077#1088#1093#1085#1080#1081' '#1087#1088#1077#1076#1077#1083'            ' +
          '                 '#1058#1080#1087' '#1076#1072#1090#1095#1080#1082#1072
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clGrayText
        Font.Height = -16
        Font.Name = 'Scada'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lbl1: TLabel
        Left = 109
        Top = 241
        Width = 258
        Height = 32
        Alignment = taRightJustify
        Caption = #1056#1077#1072#1083#1100#1085#1099#1081' '#1085#1086#1084#1077#1088' '#1088#1072#1089#1090#1086#1087#1086#1095#1085#1086#1081' '#1075#1086#1088#1077#1083#1082#1080#13#10'1-'#1103' '#1074' '#1075#1088#1091#1087#1087#1077':'
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial CYR'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lbl5: TLabel
        Left = 109
        Top = 214
        Width = 64
        Height = 20
        Caption = #1043#1088#1091#1087#1087#1072' 1'
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Scada'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Panel18: TPanel
        Left = 0
        Top = 0
        Width = 1058
        Height = 34
        Align = alTop
        BevelOuter = bvLowered
        Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1085#1086#1088#1084#1080#1088#1086#1074#1072#1085#1080#1103' '#1072#1085#1072#1083#1086#1075#1086#1074#1099#1093' '#1089#1080#1075#1085#1072#1083#1086#1074' (AST1'#8212'AST6)'
        Color = 16744448
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWhite
        Font.Height = -21
        Font.Name = 'Calibri'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
      end
      object SLAParamRegNum61: TSLAParamRegNum
        Left = 32
        Top = 99
        Width = 401
        Height = 35
        Hint = 'AST1.min'
        BothWidth = 100
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'AST1.min'
        DataPlace.rByte = -1
        Calibrate.LimitMin = -100000.000000000000000000
        Calibrate.LimitMax = 100000.000000000000000000
        Description.Alignment = taLeftJustify
        Description.Caption.Strings = (
          'AST1. '#1058#1077#1084#1087#1077#1088#1072#1090#1091#1088#1072' '#1075#1072#1079#1072)
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = 1973790
        Description.Font.Height = -15
        Description.Font.Name = 'Scada'
        Description.Font.Style = []
        Decimal = 1
        FlashColor = 33023
        FlashFont.Charset = RUSSIAN_CHARSET
        FlashFont.Color = 1973790
        FlashFont.Height = -19
        FlashFont.Name = 'Scada'
        FlashFont.Style = [fsBold]
        EditColor = clWhite
        EditFont.Charset = RUSSIAN_CHARSET
        EditFont.Color = 1973790
        EditFont.Height = -19
        EditFont.Name = 'Scada'
        EditFont.Style = [fsBold]
        SetEqualEdit = True
        TabStop = True
        TabOrder = 0
        ShowHint = True
      end
      object SLAParamRegNum62: TSLAParamRegNum
        Left = 442
        Top = 99
        Width = 235
        Height = 35
        Hint = 'AST1.max'
        BothWidth = 100
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'AST1.max'
        DataPlace.rByte = -1
        Calibrate.LimitMin = -100000.000000000000000000
        Calibrate.LimitMax = 100000.000000000000000000
        Description.Alignment = taLeftJustify
        Description.Caption.Strings = (
          'AIN 1'
          #1056#1077#1079#1077#1088#1074)
        Description.Enabled = False
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = 1973790
        Description.Font.Height = -16
        Description.Font.Name = 'Scada'
        Description.Font.Style = []
        Decimal = 1
        FlashColor = 33023
        FlashFont.Charset = RUSSIAN_CHARSET
        FlashFont.Color = 1973790
        FlashFont.Height = -19
        FlashFont.Name = 'Scada'
        FlashFont.Style = [fsBold]
        EditColor = clWhite
        EditFont.Charset = RUSSIAN_CHARSET
        EditFont.Color = 1973790
        EditFont.Height = -19
        EditFont.Name = 'Scada'
        EditFont.Style = [fsBold]
        SetEqualEdit = True
        TabStop = True
        TabOrder = 1
        ShowHint = True
      end
      object SLAParamRegTemp1: TSLAParamRegTemp
        Left = 690
        Top = 90
        Width = 216
        Height = 57
        Hint = 'AST1.type'
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'AST1.type'
        DataPlace.rByte = -1
        Description.Caption.Strings = (
          #1054#1087#1080#1089#1072#1085#1080#1077
          #1087#1072#1088#1072#1084#1077#1090#1088#1072)
        Description.Enabled = False
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = clBlack
        Description.Font.Height = -13
        Description.Font.Name = 'Tahoma'
        Description.Font.Style = []
        FlashValue = 0
        EditValue = 0
        SetEqualEdit = True
        TabStop = True
        TabOrder = 2
        ShowHint = True
      end
      object SLAParamRegOpt63: TSLAParamRegOpt
        Left = 915
        Top = 91
        Width = 109
        Height = 44
        Hint = 'AST1.coldjunction'
        OptionStyle = osCross
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'AST1.coldjunction'
        DataPlace.rByte = -1
        DataPlace.rBit = -1
        Description.Alignment = taLeftJustify
        Description.Caption.Strings = (
          #1050#1086#1084#1087#1077#1085#1089#1072#1094#1080#1103
          #1093#1086#1083#1086#1076#1085#1086#1075#1086
          #1089#1087#1072#1103)
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = clBlack
        Description.Font.Height = -12
        Description.Font.Name = 'Scada'
        Description.Font.Style = []
        FlashValue = False
        EditValue = False
        SetEqualEdit = True
        ShowHint = True
      end
      object SLAParamRegNum57: TSLAParamRegNum
        Left = 375
        Top = 241
        Width = 91
        Height = 35
        Hint = 'Group1_1'
        BothWidth = 28
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'Group1_1'
        DataPlace.rByte = -1
        Calibrate.LimitMin = -100000.000000000000000000
        Calibrate.LimitMax = 100000.000000000000000000
        Description.Alignment = taLeftJustify
        Description.Caption.Strings = (
          '')
        Description.Enabled = False
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = clBlack
        Description.Font.Height = -15
        Description.Font.Name = 'Scada'
        Description.Font.Style = []
        Decimal = 0
        FlashColor = 33023
        FlashFont.Charset = RUSSIAN_CHARSET
        FlashFont.Color = clBlack
        FlashFont.Height = -19
        FlashFont.Name = 'Scada'
        FlashFont.Style = [fsBold]
        EditColor = clBtnFace
        EditFont.Charset = RUSSIAN_CHARSET
        EditFont.Color = clBlack
        EditFont.Height = -19
        EditFont.Name = 'Scada'
        EditFont.Style = [fsBold]
        SetEqualEdit = True
        TabStop = True
        TabOrder = 5
        ShowHint = True
      end
      object SLAParamRegOpt16: TSLAParamRegOpt
        Left = 603
        Top = 197
        Width = 381
        Height = 50
        Hint = 'bFwaterMinInv'
        OptionStyle = osZeroOne
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'bFwaterMinInv'
        DataPlace.rByte = -1
        DataPlace.rBit = -1
        Description.Alignment = taLeftJustify
        Description.AutoHorizontal = True
        Description.Caption.Strings = (
          #1080#1089#1087#1086#1083#1100#1079#1091#1077#1090#1089#1103' '#1076#1083#1103' '#1079#1072#1076#1072#1085#1080#1103' '#1072#1074#1072#1088#1080#1081#1085#1086#1075#1086' '#1089#1086#1089#1090#1086#1103#1085#1080#1103' '#1076#1072#1090#1095#1080#1082#1072' '
          #1087#1086#1085#1080#1078#1077#1085#1080#1103' '#1088#1072#1089#1093#1086#1076#1072' '#1074#1086#1076#1099', '#1087#1088#1080' "0" '#1072#1074#1072#1088#1080#1081#1085#1099#1084' '#1089#1086#1089#1090#1086#1103#1085#1080#1077#1084' '
          #1073#1091#1076#1077#1090' '#1079#1072#1084#1099#1082#1072#1085#1080#1077' '#1082#1086#1085#1090#1072#1082#1090#1072' '#1076#1072#1090#1095#1080#1082#1072', '#1087#1088#1080' "1" - '#1088#1072#1079#1084#1099#1082#1072#1085#1080#1077'.')
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = clBlack
        Description.Font.Height = -13
        Description.Font.Name = 'Scada'
        Description.Font.Style = []
        FlashValue = False
        EditValue = False
        SetEqualEdit = True
        ShowHint = True
      end
      object SLAParamRegOpt17: TSLAParamRegOpt
        Left = 603
        Top = 286
        Width = 392
        Height = 50
        Hint = 'bPwaterMaxInv'
        OptionStyle = osZeroOne
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'bPwaterMaxInv'
        DataPlace.rByte = -1
        DataPlace.rBit = -1
        Description.Alignment = taLeftJustify
        Description.AutoHorizontal = True
        Description.Caption.Strings = (
          #1080#1089#1087#1086#1083#1100#1079#1091#1077#1090#1089#1103' '#1076#1083#1103' '#1079#1072#1076#1072#1085#1080#1103' '#1072#1074#1072#1088#1080#1081#1085#1086#1075#1086' '#1089#1086#1089#1090#1086#1103#1085#1080#1103' '#1076#1072#1090#1095#1080#1082#1072' '
          #1087#1086#1074#1099#1096#1077#1085#1080#1103' '#1076#1072#1074#1083#1077#1085#1080#1103' '#1074#1086#1076#1099', '#1087#1088#1080' "0" '#1072#1074#1072#1088#1080#1081#1085#1099#1084' '#1089#1086#1089#1090#1086#1103#1085#1080#1077#1084' '
          #1073#1091#1076#1077#1090' '#1079#1072#1084#1099#1082#1072#1085#1080#1077' '#1082#1086#1085#1090#1072#1082#1090#1072' '#1076#1072#1090#1095#1080#1082#1072', '#1087#1088#1080' "1" - '#1088#1072#1079#1084#1099#1082#1072#1085#1080#1077'.')
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = clBlack
        Description.Font.Height = -13
        Description.Font.Name = 'Scada'
        Description.Font.Style = []
        FlashValue = False
        EditValue = False
        SetEqualEdit = True
        ShowHint = True
      end
      object SLAParamRegOpt56: TSLAParamRegOpt
        Left = 603
        Top = 375
        Width = 390
        Height = 50
        Hint = 'bPwaterMinInv'
        OptionStyle = osZeroOne
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'bPwaterMinInv'
        DataPlace.rByte = -1
        DataPlace.rBit = -1
        Description.Alignment = taLeftJustify
        Description.AutoHorizontal = True
        Description.Caption.Strings = (
          #1080#1089#1087#1086#1083#1100#1079#1091#1077#1090#1089#1103' '#1076#1083#1103' '#1079#1072#1076#1072#1085#1080#1103' '#1072#1074#1072#1088#1080#1081#1085#1086#1075#1086' '#1089#1086#1089#1090#1086#1103#1085#1080#1103' '#1076#1072#1090#1095#1080#1082#1072' '
          #1087#1086#1085#1080#1078#1077#1085#1080#1103' '#1076#1072#1074#1083#1077#1085#1080#1103' '#1074#1086#1076#1099', '#1087#1088#1080' "0" '#1072#1074#1072#1088#1080#1081#1085#1099#1084' '#1089#1086#1089#1090#1086#1103#1085#1080#1077#1084' '
          #1073#1091#1076#1077#1090' '#1079#1072#1084#1099#1082#1072#1085#1080#1077' '#1082#1086#1085#1090#1072#1082#1090#1072' '#1076#1072#1090#1095#1080#1082#1072', '#1087#1088#1080' "1" - '#1088#1072#1079#1084#1099#1082#1072#1085#1080#1077'.')
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = clBlack
        Description.Font.Height = -13
        Description.Font.Name = 'Scada'
        Description.Font.Style = []
        FlashValue = False
        EditValue = False
        SetEqualEdit = True
        ShowHint = True
      end
      object SLAParamRegOpt18: TSLAParamRegOpt
        Left = 603
        Top = 464
        Width = 412
        Height = 50
        Hint = 'bTwaterMaxInv'
        OptionStyle = osZeroOne
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'bTwaterMaxInv'
        DataPlace.rByte = -1
        DataPlace.rBit = -1
        Description.Alignment = taLeftJustify
        Description.AutoHorizontal = True
        Description.Caption.Strings = (
          #1080#1089#1087#1086#1083#1100#1079#1091#1077#1090#1089#1103' '#1076#1083#1103' '#1079#1072#1076#1072#1085#1080#1103' '#1072#1074#1072#1088#1080#1081#1085#1086#1075#1086' '#1089#1086#1089#1090#1086#1103#1085#1080#1103' '#1076#1072#1090#1095#1080#1082#1072' '
          #1087#1086#1074#1099#1096#1077#1085#1080#1103' '#1090#1077#1084#1087#1077#1088#1072#1090#1091#1088#1099' '#1074#1086#1076#1099', '#1087#1088#1080' "0" '#1072#1074#1072#1088#1080#1081#1085#1099#1084' '#1089#1086#1089#1090#1086#1103#1085#1080#1077#1084' '
          #1073#1091#1076#1077#1090' '#1079#1072#1084#1099#1082#1072#1085#1080#1077' '#1082#1086#1085#1090#1072#1082#1090#1072' '#1076#1072#1090#1095#1080#1082#1072', '#1087#1088#1080' "1" - '#1088#1072#1079#1084#1099#1082#1072#1085#1080#1077'.')
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = clBlack
        Description.Font.Height = -13
        Description.Font.Name = 'Scada'
        Description.Font.Style = []
        FlashValue = False
        EditValue = False
        SetEqualEdit = True
        ShowHint = True
      end
      object SLAParamRegScrl26: TSLAParamRegScrl
        Left = 79
        Top = 404
        Width = 387
        Height = 44
        Hint = 'RegVacuum.Kus'
        DataPlace.dpIndex = -1
        DataPlace.dpName = 'RegVacuum.Kus'
        DataPlace.rByte = -1
        Calibrate.Max = 25.500000000000000000
        Calibrate.LimitMax = 25.500000000000000000
        Calibrate.LimitColor = clRed
        Description.Alignment = taLeftJustify
        Description.Caption.Strings = (
          #1050#1086#1101#1092#1092#1080#1094#1080#1077#1085#1090' '#1091#1089#1080#1083#1077#1085#1080#1103)
        Description.Font.Charset = RUSSIAN_CHARSET
        Description.Font.Color = clBlack
        Description.Font.Height = -15
        Description.Font.Name = 'Scada'
        Description.Font.Style = []
        GaugeFont.Charset = RUSSIAN_CHARSET
        GaugeFont.Color = clWhite
        GaugeFont.Height = -11
        GaugeFont.Name = 'Tahoma'
        GaugeFont.Style = []
        DimentionStr = ' '
        Decimal = 1
        EditColor = clSilver
        EditFont.Charset = RUSSIAN_CHARSET
        EditFont.Color = clBlack
        EditFont.Height = -19
        EditFont.Name = 'Tahoma'
        EditFont.Style = [fsBold]
        EditValue = 1.000000000000000000
        EditWidth = 70
        ScaleColor = 8618364
        SetEqualEdit = True
        TabStop = True
        TabOrder = 10
        ShowHint = True
      end
    end
  end
  object RFlashTimer: TTimer
    Enabled = False
    Interval = 300
    Left = 10
    Top = 98
  end
  object sdParams: TSaveDialog
    DefaultExt = '*.eisafp'
    Filter = #1060#1072#1081#1083' '#1087#1072#1088#1072#1084#1077#1090#1088#1086#1074' '#1088#1077#1075#1091#1083#1103#1090#1086#1088#1086#1074' (*.eisafp)|*.eisafp'
    Left = 75
    Top = 98
  end
  object odParams: TOpenDialog
    DefaultExt = '*.eisafp'
    Filter = #1060#1072#1081#1083' '#1087#1072#1088#1072#1084#1077#1090#1088#1086#1074' '#1088#1077#1075#1091#1083#1103#1090#1086#1088#1086#1074' (*.eisafp)|*.eisafp'
    Left = 43
    Top = 98
  end
end
