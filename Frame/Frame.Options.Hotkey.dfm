inherited frmHotkeyOptionsPage: TfrmHotkeyOptionsPage
  Height = 405
  ExplicitHeight = 405
  object gbHotkey: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 439
    Height = 118
    Align = alTop
    Caption = 'Hotkey'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lblHotkeyWindow: TLabel
      Left = 8
      Top = 39
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
    object lblHotkeyGM: TLabel
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
    object lblHotkeyCM: TLabel
      Left = 8
      Top = 94
      Width = 207
      Height = 13
      Caption = 'Show classic menu when hotkey is pressed'
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
    object edtHotkeyMF: TButtonedEdit
      Left = 248
      Top = 37
      Width = 179
      Height = 21
      CharCase = ecUpperCase
      LeftButton.Enabled = False
      ReadOnly = True
      RightButton.Visible = True
      TabOrder = 1
      OnChange = edtHotkeyChange
      OnClick = edtHotkeyClick
      OnRightButtonClick = edtHotkeyButtonClick
    end
    object edtHotkeyGM: TButtonedEdit
      Left = 248
      Top = 64
      Width = 179
      Height = 21
      CharCase = ecUpperCase
      LeftButton.Enabled = False
      ReadOnly = True
      RightButton.Visible = True
      TabOrder = 2
      OnChange = edtHotkeyChange
      OnRightButtonClick = edtHotkeyButtonClick
    end
    object edtHotkeyCM: TButtonedEdit
      Left = 248
      Top = 91
      Width = 179
      Height = 21
      CharCase = ecUpperCase
      LeftButton.Enabled = False
      ReadOnly = True
      RightButton.Visible = True
      TabOrder = 3
      OnChange = edtHotkeyChange
      OnRightButtonClick = edtHotkeyButtonClick
    end
  end
  object grpOrderSoftware: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 127
    Width = 439
    Height = 273
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
      Left = 5
      Top = 18
      Width = 429
      Height = 250
      Align = alClient
      DefaultNodeHeight = 32
      DragOperations = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Header.AutoSizeIndex = 0
      Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoVisible, hoHeaderClickAutoSort]
      ParentFont = False
      PopupMenu = pmHotkey
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toPopupMode, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
      ExplicitLeft = 8
      ExplicitTop = 22
      ExplicitWidth = 417
      ExplicitHeight = 244
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
          MinWidth = 60
          Position = 2
          Text = 'Category'
          Width = 100
        end
        item
          MinWidth = 100
          Position = 3
          Text = 'Hotkey'
          Width = 200
        end>
    end
  end
  object DKLanguageController1: TDKLanguageController
    IgnoreList.Strings = (
      '*.Items')
    Left = 8
    Top = 248
    LangData = {
      140066726D486F746B65794F7074696F6E735061676500010F00000008006762
      486F746B6579010100000001000000070043617074696F6E000F006C626C486F
      746B657957696E646F77010100000002000000070043617074696F6E000B006C
      626C486F746B6579474D010100000003000000070043617074696F6E00080063
      62486F744B6579010100000004000000070043617074696F6E0010006772704F
      72646572536F667477617265010100000005000000070043617074696F6E0008
      007673744974656D7300000800706D486F746B657900000D006D6E6945646974
      486F746B6579010100000006000000070043617074696F6E000F006D6E695265
      6D6F7665486F746B6579010100000007000000070043617074696F6E0005006D
      6E694E3100000D006D6E6950726F706572746965730101000000080000000700
      43617074696F6E000B006C626C486F746B6579434D0101000000090000000700
      43617074696F6E000B00656474486F746B65794D4600000B00656474486F746B
      6579474D00000B00656474486F746B6579434D0000}
  end
  object pmHotkey: TPopupMenu
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
