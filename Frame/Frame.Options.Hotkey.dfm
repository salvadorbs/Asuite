inherited frmHotkeyOptionsPage: TfrmHotkeyOptionsPage
  Height = 405
  ExplicitHeight = 405
  object gbHotkey: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 439
    Height = 94
    Align = alTop
    Caption = 'Hotkey'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 42
      Width = 181
      Height = 13
      Caption = 'Show window when hotkey is pressed'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 67
      Width = 209
      Height = 13
      Caption = 'Show graphic menu when hotkey is pressed'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object cbHotKey: TCheckBox
      Left = 8
      Top = 16
      Width = 419
      Height = 17
      Caption = 'Enable hotkey'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = cbHotKeyClick
    end
    object hkWindow: THotKey
      Left = 248
      Top = 39
      Width = 179
      Height = 19
      HotKey = 0
      Modifiers = []
      TabOrder = 1
      OnChange = hkWindowChange
      OnMouseUp = hkWindowMouseUp
    end
    object hkGraphicMenu: THotKey
      Left = 248
      Top = 64
      Width = 179
      Height = 19
      HotKey = 0
      Modifiers = []
      TabOrder = 2
      OnChange = hkGraphicMenuChange
      OnMouseUp = hkGraphicMenuMouseUp
    end
  end
  object grpOrderSoftware: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 103
    Width = 439
    Height = 297
    Margins.Bottom = 5
    Align = alClient
    Caption = 'Hotkey list used'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object vstItems: TVirtualStringTree
      AlignWithMargins = True
      Left = 10
      Top = 20
      Width = 417
      Height = 268
      Margins.Left = 8
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 7
      Align = alClient
      DefaultNodeHeight = 32
      DragOperations = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoVisible, hoHeaderClickAutoSort]
      Images = dmImages.ilSmallIcons
      ParentFont = False
      PopupMenu = pmHotkey
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toPopupMode, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
      Columns = <
        item
          MinWidth = 50
          Position = 0
          WideText = 'Name'
        end
        item
          MaxWidth = 36
          MinWidth = 36
          Position = 1
          Width = 36
          WideText = 'Type'
        end
        item
          MinWidth = 60
          Position = 2
          Width = 100
          WideText = 'Category'
        end
        item
          MinWidth = 100
          Position = 3
          Width = 200
          WideText = 'Hotkey'
        end>
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Items')
    Left = 8
    Top = 248
    LangData = {
      140066726D486F746B65794F7074696F6E735061676500010D00000008006762
      486F746B6579010100000001000000070043617074696F6E0006004C6162656C
      31010100000002000000070043617074696F6E0006004C6162656C3201010000
      0003000000070043617074696F6E0008006362486F744B657901010000000400
      0000070043617074696F6E000800686B57696E646F7700000D00686B47726170
      6869634D656E75000010006772704F72646572536F6674776172650101000000
      05000000070043617074696F6E0008007673744974656D7300000800706D486F
      746B657900000D006D6E6945646974486F746B65790101000000060000000700
      43617074696F6E000F006D6E6952656D6F7665486F746B657901010000000700
      0000070043617074696F6E0005006D6E694E3100000D006D6E6950726F706572
      74696573010100000008000000070043617074696F6E00}
  end
  object pmHotkey: TPopupMenu
    Images = dmImages.ilSmallIcons
    Left = 264
    Top = 200
    object mniEditHotkey: TMenuItem
      Caption = 'Edit Hotkey'
      OnClick = mniEditHotkeyClick
    end
    object mniRemoveHotkey: TMenuItem
      Caption = 'Remove Hotkey'
      OnClick = mniRemoveHotkeyClick
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
