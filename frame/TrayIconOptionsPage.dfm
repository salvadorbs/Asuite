inherited frmTrayiconOptionsPage: TfrmTrayiconOptionsPage
  object gbTrayicon: TGroupBox [0]
    Left = 3
    Top = 3
    Width = 320
    Height = 158
    Caption = 'System Tray'
    TabOrder = 0
    object lbTrayLeftClick: TLabel
      Left = 8
      Top = 106
      Width = 41
      Height = 13
      Caption = 'Left click'
      Color = clBtnFace
      ParentColor = False
    end
    object lbTrayRightClick: TLabel
      Left = 164
      Top = 106
      Width = 47
      Height = 13
      Caption = 'Right click'
      Color = clBtnFace
      ParentColor = False
    end
    object cxLeftClick: TComboBox
      Left = 8
      Top = 119
      Width = 150
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      Items.Strings = (
        'No action'
        'Show window'
        'Show menu')
    end
    object btnBrowse: TButton
      Left = 255
      Top = 58
      Width = 55
      Height = 21
      Caption = 'Browse'
      TabOrder = 3
      OnClick = btnBrowseClick
    end
    object cbTrayicon: TCheckBox
      Left = 8
      Top = 20
      Width = 156
      Height = 19
      Caption = 'Enable the System Tray icon'
      TabOrder = 0
      OnClick = cbTrayiconClick
    end
    object cxRightClick: TComboBox
      Left = 162
      Top = 119
      Width = 150
      Height = 21
      Style = csDropDownList
      TabOrder = 6
      Items.Strings = (
        'No action'
        'Show window'
        'Show menu')
    end
    object cbTrayCustomIcon: TCheckBox
      Left = 8
      Top = 39
      Width = 101
      Height = 19
      Caption = 'Custom tray icon'
      TabOrder = 1
      OnClick = cbTrayCustomIconClick
    end
    object cbClassicMenu: TCheckBox
      Left = 8
      Top = 79
      Width = 302
      Height = 17
      Caption = 'Use classic menu'
      TabOrder = 4
      OnClick = cbClassicMenuClick
    end
    object edtCustomIcon: TEdit
      Left = 8
      Top = 58
      Width = 241
      Height = 21
      TabOrder = 2
    end
  end
  object grpGraphicMenu: TGroupBox [1]
    Left = 3
    Top = 167
    Width = 158
    Height = 101
    Caption = 'Graphic Menu'
    TabOrder = 1
    object lbMenuTheme: TLabel
      Left = 8
      Top = 42
      Width = 32
      Height = 13
      Caption = 'Theme'
    end
    object cxTheme: TComboBox
      Left = 8
      Top = 58
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object cbMenuFade: TCheckBox
      Left = 8
      Top = 17
      Width = 241
      Height = 19
      Caption = 'Enable fade menu'
      TabOrder = 0
    end
  end
  object grpClassicMenu: TGroupBox [2]
    Left = 165
    Top = 167
    Width = 158
    Height = 101
    Caption = 'Classic Menu'
    TabOrder = 2
    object cbSubMenuMFU: TCheckBox
      Left = 3
      Top = 18
      Width = 144
      Height = 19
      Caption = 'MFU on trayicon submenu'
      TabOrder = 0
    end
    object cbSubMenuMRU: TCheckBox
      Left = 3
      Top = 34
      Width = 145
      Height = 19
      Caption = 'MRU on trayicon submenu'
      TabOrder = 1
    end
  end
end
