object frmEEPROMreport: TfrmEEPROMreport
  Left = 987
  Top = 290
  Width = 253
  Height = 119
  Caption = 'frmEEPROMreport'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ReportEEPROM: TfrxReport
    Version = '4.9.32'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.Buttons = [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbEdit, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.Author = #1053#1072#1083#1072#1076#1095#1080#1082' '#1042#1086#1074#1072
    ReportOptions.CreateDate = 43161.413371851900000000
    ReportOptions.LastChange = 43173.666613576400000000
    ScriptLanguage = 'PascalScript'
    ScriptText.Strings = (
      'var'
      '  tankType: string;'
      
        '  tankNumber: string;                                           ' +
        '    '
      'begin'
      '  if tankType = '#39#39' then '
      '    tankType := '#39'<b>"'#1090#1080#1087' '#1085#1077' '#1091#1082#1072#1079#1072#1085'"</b>'#39';'
      '  if tankNumber = '#39#39' then'
      
        '    tankNumber := '#39'<b>"'#1085#1086#1084#1077#1088' '#1085#1077' '#1091#1082#1072#1079#1072#1085'"</b>'#39';                   ' +
        '      '
      'end.')
    Left = 40
    Top = 16
    Datasets = <
      item
        DataSet = udsValues
        DataSetName = 'Values'
      end>
    Variables = <>
    Style = <>
    object Data: TfrxDataPage
      Height = 1000.000000000000000000
      Width = 1000.000000000000000000
    end
    object Page1: TfrxReportPage
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Droid Sans'
      Font.Style = []
      PaperWidth = 210.000000000000000000
      PaperHeight = 297.000000000000000000
      PaperSize = 9
      LeftMargin = 10.000000000000000000
      RightMargin = 10.000000000000000000
      TopMargin = 10.000000000000000000
      BottomMargin = 10.000000000000000000
      HGuides.Strings = (
        '37,7953'
        '26,45671')
      object rtEEPROM: TfrxReportTitle
        Height = 207.874150000000000000
        Top = 18.897650000000000000
        Width = 718.110700000000000000
        object Memo2: TfrxMemoView
          Top = 94.488250000000000000
          Width = 718.110700000000000000
          Height = 30.236240000000000000
          ShowHint = False
          AllowHTMLTags = True
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clBlack
          Font.Height = -16
          Font.Name = 'Droid Sans'
          Font.Style = []
          Frame.Style = fsDot
          Frame.Width = 0.100000000000000000
          HAlign = haCenter
          Memo.UTF8 = (
            
              #1057#1107#1057#1027#1057#8218#1056#176#1056#1030#1056#1109#1056#1108' '#1056#1105' '#1056#1029#1056#176#1057#1027#1057#8218#1057#1026#1056#1109#1056#181#1056#1108' '#1056#1108#1056#1109#1057#8218#1056#187#1056#176' [tankType] '#1057#1027#1057#8218'. '#1074 +
              #8222#8211' [tankNumber]')
          ParentFont = False
          VAlign = vaCenter
        end
        object Memo3: TfrxMemoView
          Top = 143.622140000000000000
          Width = 151.181200000000000000
          Height = 26.456710000000000000
          ShowHint = False
          AllowHTMLTags = True
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clBlack
          Font.Height = -16
          Font.Name = 'Droid Sans'
          Font.Style = []
          Frame.Style = fsDot
          Frame.Width = 0.100000000000000000
          HAlign = haCenter
          Memo.UTF8 = (
            '<u>  [DATE]  </u>')
          ParentFont = False
          VAlign = vaCenter
        end
        object Memo1: TfrxMemoView
          Top = 45.354360000000000000
          Width = 718.110700000000000000
          Height = 37.795300000000000000
          ShowHint = False
          AllowHTMLTags = True
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clBlack
          Font.Height = -21
          Font.Name = 'Droid Sans'
          Font.Style = [fsBold]
          Frame.Style = fsDot
          Frame.Width = 0.100000000000000000
          HAlign = haCenter
          Memo.UTF8 = (
            #1056#1106#1056#1113#1056#1118)
          ParentFont = False
        end
      end
      object pfEEPROM: TfrxPageFooter
        Height = 22.677180000000000000
        Top = 551.811380000000000000
        Width = 718.110700000000000000
        object Memo5: TfrxMemoView
          Width = 377.953000000000000000
          Height = 22.677180000000000000
          Visible = False
          ShowHint = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Georgia'
          Font.Style = [fsItalic]
          Frame.Style = fsDot
          Frame.Width = 0.100000000000000000
          Memo.UTF8 = (
            '[Date]    [Time]      '#1056#1106#1056#1108#1057#8218' '#1057#1107#1057#1027#1057#8218#1056#176#1056#1030#1056#1109#1056#1108' '#1056#1105' '#1056#1029#1056#176#1057#1027#1057#8218#1057#1026#1056#1109#1056#181#1056#1108)
          ParentFont = False
          VAlign = vaCenter
        end
        object SysMemo1: TfrxSysMemoView
          Left = 616.063390000000000000
          Width = 102.047310000000000000
          Height = 22.677180000000000000
          ShowHint = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -15
          Font.Name = 'Droid Sans'
          Font.Style = []
          HAlign = haRight
          Memo.UTF8 = (
            '[PAGE#]')
          ParentFont = False
          VAlign = vaCenter
        end
      end
      object ReportSummary1: TfrxReportSummary
        Height = 98.267780000000000000
        Top = 430.866420000000000000
        Width = 718.110700000000000000
        object Memo4: TfrxMemoView
          Left = 39.149660000000000000
          Top = 60.472480000000080000
          Width = 415.748300000000000000
          Height = 22.677180000000000000
          ShowHint = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clBlack
          Font.Height = -16
          Font.Name = 'Georgia'
          Font.Style = []
          Frame.Style = fsDot
          Frame.Width = 0.100000000000000000
          HAlign = haCenter
          Memo.UTF8 = (
            '______________________________________________')
          ParentFont = False
        end
        object Memo6: TfrxMemoView
          Left = 491.338900000000000000
          Top = 60.472480000000080000
          Width = 188.976500000000000000
          Height = 22.677180000000000000
          ShowHint = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clBlack
          Font.Height = -16
          Font.Name = 'Georgia'
          Font.Style = []
          Frame.Style = fsDot
          Frame.Width = 0.100000000000000000
          HAlign = haCenter
          Memo.UTF8 = (
            '_______________________')
          ParentFont = False
        end
        object Memo7: TfrxMemoView
          Left = 39.149660000000000000
          Top = 83.149659999999870000
          Width = 415.748300000000000000
          Height = 15.118120000000000000
          ShowHint = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Droid Sans'
          Font.Style = []
          Frame.Style = fsDot
          Frame.Width = 0.100000000000000000
          HAlign = haCenter
          Memo.UTF8 = (
            #1056#8221#1056#1109#1056#187#1056#182#1056#1029#1056#1109#1057#1027#1057#8218#1057#1034' '#1056#1105' '#1056#164'.'#1056#152'.'#1056#1115'.')
          ParentFont = False
        end
        object Memo8: TfrxMemoView
          Left = 491.338900000000000000
          Top = 83.149659999999870000
          Width = 188.976500000000000000
          Height = 15.118120000000000000
          ShowHint = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Droid Sans'
          Font.Style = []
          Frame.Style = fsDot
          Frame.Width = 0.100000000000000000
          HAlign = haCenter
          Memo.UTF8 = (
            #1056#1111#1056#1109#1056#1169#1056#1111#1056#1105#1057#1027#1057#1034)
          ParentFont = False
        end
      end
      object GroupHeader1: TfrxGroupHeader
        Height = 37.795300000000000000
        Top = 287.244280000000000000
        Width = 718.110700000000000000
        Condition = 'Values."index"'
        Stretched = True
        object Memo20: TfrxMemoView
          Width = 718.110700000000000000
          Height = 37.795300000000000000
          ShowHint = False
          StretchMode = smMaxHeight
          Color = clSilver
          DataSetName = 'Groups'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -15
          Font.Name = 'Droid Sans'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            '      [Values."groupName"]')
          ParentFont = False
          VAlign = vaCenter
        end
      end
      object MasterData1: TfrxMasterData
        Height = 22.677180000000000000
        Top = 347.716760000000000000
        Width = 718.110700000000000000
        DataSet = udsValues
        DataSetName = 'Values'
        RowCount = 0
        Stretched = True
        object Memo19: TfrxMemoView
          Width = 718.110700000000000000
          Height = 22.677180000000000000
          ShowHint = False
          StretchMode = smMaxHeight
          DataSet = udsValues
          DataSetName = 'Values'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Droid Sans'
          Font.Style = []
          Frame.Typ = [ftTop, ftBottom]
          Frame.TopLine.Style = fsDot
          Frame.BottomLine.Style = fsDot
          Highlight.Font.Charset = DEFAULT_CHARSET
          Highlight.Font.Color = clBlack
          Highlight.Font.Height = -13
          Highlight.Font.Name = 'Droid Sans'
          Highlight.Font.Style = []
          Highlight.Color = 15329769
          Highlight.Condition = '<Line> mod 2 = 0'
          ParentFont = False
          VAlign = vaCenter
        end
        object Memo17: TfrxMemoView
          Left = 75.590600000000000000
          Width = 377.953000000000000000
          Height = 22.677180000000000000
          ShowHint = False
          StretchMode = smMaxHeight
          DataSet = udsValues
          DataSetName = 'Values'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Droid Sans'
          Font.Style = []
          Frame.TopLine.Style = fsDot
          Frame.BottomLine.Style = fsDot
          Memo.UTF8 = (
            '[Values."valueName"]')
          ParentFont = False
          VAlign = vaCenter
        end
        object Memo18: TfrxMemoView
          Left = 491.338900000000000000
          Width = 226.771800000000000000
          Height = 22.677180000000000000
          ShowHint = False
          StretchMode = smMaxHeight
          DataSet = udsValues
          DataSetName = 'Values'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Droid Sans'
          Font.Style = []
          Frame.Style = fsDot
          Memo.UTF8 = (
            '[Values."value"]')
          ParentFont = False
          VAlign = vaCenter
        end
      end
    end
  end
  object udsValues: TfrxUserDataSet
    Description = #1044#1072#1085#1085#1099#1077' '#1085#1072#1089#1090#1088#1086#1077#1082' EEPROM'
    UserName = 'Values'
    OnCheckEOF = udsValuesCheckEOF
    OnFirst = udsValuesFirst
    OnNext = udsValuesNext
    Fields.Strings = (
      'index'
      'groupName'
      'valueName'
      'value')
    OnGetValue = udsValuesGetValue
    Left = 192
    Top = 16
  end
end
