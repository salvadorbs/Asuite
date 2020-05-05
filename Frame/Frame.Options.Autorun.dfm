inherited frmAutorunOptionsPage: TfrmAutorunOptionsPage
  Height = 405
  ExplicitHeight = 405
  object grpStartupOrderItems: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 439
    Height = 200
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Startup'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    DesignSize = (
      439
      200)
    object lblStartupInfo: TLabel
      Left = 8
      Top = 43
      Width = 193
      Height = 13
      Caption = 'Order of startup autorun items in ASuite'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object chkStartup: TCheckBox
      Left = 8
      Top = 20
      Width = 409
      Height = 19
      Caption = 'Enable startup autorun'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object vstStartupItems: TVirtualStringTree
      AlignWithMargins = True
      Left = 5
      Top = 59
      Width = 394
      Height = 136
      Margins.Right = 38
      Align = alBottom
      DefaultNodeHeight = 32
      DragOperations = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Header.AutoSizeIndex = 0
      Header.Options = [hoColumnResize, hoVisible]
      ParentFont = False
      PopupMenu = pmAutorun
      TabOrder = 1
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toPopupMode, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
      TreeOptions.SelectionOptions = [toFullRowSelect]
      OnGetPopupMenu = vstGetPopupMenu
      Columns = <
        item
          MinWidth = 50
          Position = 0
          Text = 'Name'
        end
        item
          MaxWidth = 100
          MinWidth = 36
          Position = 1
          Text = 'Type'
          Width = 36
        end
        item
          MinWidth = 100
          Position = 2
          Text = 'Category'
          Width = 100
        end
        item
          MinWidth = 100
          Position = 3
          Text = 'Path executable'
          Width = 200
        end>
    end
    object btnStartupUp: TKSpeedButton
      Left = 405
      Top = 88
      Width = 27
      Height = 25
      Anchors = [akTop]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnStartupUpClick
    end
    object btnStartupDelete: TKSpeedButton
      Left = 405
      Top = 119
      Width = 27
      Height = 25
      Anchors = [akTop]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = btnStartupDeleteClick
    end
    object btnStartupDown: TKSpeedButton
      Left = 405
      Top = 150
      Width = 27
      Height = 25
      Anchors = [akTop]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnStartupDownClick
    end
  end
  object grpShutdownOrderItems: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 206
    Width = 439
    Height = 200
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Shutdown'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      439
      200)
    object lblShutdownInfo: TLabel
      Left = 8
      Top = 43
      Width = 205
      Height = 13
      Caption = 'Order of shutdown autorun items in ASuite'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object chkShutdown: TCheckBox
      Left = 8
      Top = 20
      Width = 409
      Height = 19
      Caption = 'Enable shutdown autorun'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object vstShutdownItems: TVirtualStringTree
      AlignWithMargins = True
      Left = 5
      Top = 59
      Width = 394
      Height = 136
      Margins.Right = 38
      Align = alBottom
      DefaultNodeHeight = 32
      DragOperations = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Header.AutoSizeIndex = 0
      Header.Options = [hoColumnResize, hoVisible]
      ParentFont = False
      PopupMenu = pmAutorun
      TabOrder = 1
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toPopupMode, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
      TreeOptions.SelectionOptions = [toFullRowSelect]
      OnGetPopupMenu = vstGetPopupMenu
      Columns = <
        item
          MinWidth = 50
          Position = 0
          Text = 'Name'
        end
        item
          MaxWidth = 100
          MinWidth = 36
          Position = 1
          Text = 'Type'
          Width = 36
        end
        item
          MinWidth = 100
          Position = 2
          Text = 'Category'
          Width = 100
        end
        item
          MinWidth = 100
          Position = 3
          Text = 'Path executable'
          Width = 200
        end>
    end
    object btnShutdownDelete: TKSpeedButton
      Left = 405
      Top = 119
      Width = 27
      Height = 25
      Anchors = [akTop]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = btnShutdownDeleteClick
    end
    object btnShutdownDown: TKSpeedButton
      Left = 405
      Top = 150
      Width = 27
      Height = 25
      Anchors = [akTop]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnShutdownDownClick
    end
    object btnShutdownUp: TKSpeedButton
      Left = 405
      Top = 88
      Width = 27
      Height = 25
      Anchors = [akTop]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnShutdownUpClick
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 384
    Top = 24
    LangData = {
      150066726D4175746F72756E4F7074696F6E7350616765000112000000140067
      7270537461727475704F726465724974656D7301010000000100000007004361
      7074696F6E00150067727053687574646F776E4F726465724974656D73010100
      000002000000070043617074696F6E000A0063686B5374617274757001010000
      0003000000070043617074696F6E000E006C626C53746172747570496E666F01
      0100000004000000070043617074696F6E000B0063686B53687574646F776E01
      0100000005000000070043617074696F6E000F006C626C53687574646F776E49
      6E666F010100000006000000070043617074696F6E000F007673745374617274
      75704974656D730000100076737453687574646F776E4974656D7300000C0062
      746E5374617274757055700000100062746E5374617274757044656C65746500
      000E0062746E53746172747570446F776E0000110062746E53687574646F776E
      44656C65746500000F0062746E53687574646F776E446F776E00000D0062746E
      53687574646F776E557000000900706D4175746F72756E000010006D6E695265
      6D6F76654175746F72756E010100000007000000070043617074696F6E000500
      6D6E694E3100000D006D6E6950726F7065727469657301010000000800000007
      0043617074696F6E00}
  end
  object pmAutorun: TPopupMenu
    Left = 304
    Top = 24
    object mniRemoveAutorun: TMenuItem
      Caption = 'Remove autorun'
      OnClick = mniRemoveAutorunClick
    end
    object mniN1: TMenuItem
      Caption = '-'
    end
    object mniProperties: TMenuItem
      Caption = 'Properties'
      OnClick = mniPropertiesClick
    end
  end
end
