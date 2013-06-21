inherited frmSensorsOptionsPage: TfrmSensorsOptionsPage
  object gbMouse: TGroupBox
    Left = 3
    Top = 3
    Width = 294
    Height = 158
    Caption = 'Mouse sensors'
    TabOrder = 0
    object lbSide: TLabel
      Left = 5
      Top = 38
      Width = 24
      Height = 13
      Caption = 'Side'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object lbLeftClick: TLabel
      Left = 88
      Top = 38
      Width = 50
      Height = 13
      Caption = 'Left click'
      Color = clBtnFace
      FocusControl = cxLCLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object lbRightClick: TLabel
      Left = 187
      Top = 38
      Width = 58
      Height = 13
      Caption = 'Right click'
      Color = clBtnFace
      FocusControl = cxRCLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object lbBottom: TLabel
      Left = 5
      Top = 130
      Width = 34
      Height = 13
      Caption = 'Bottom'
      Color = clBtnFace
      FocusControl = cxRCBottom
      ParentColor = False
    end
    object lbRight: TLabel
      Left = 5
      Top = 105
      Width = 25
      Height = 13
      Caption = 'Right'
      Color = clBtnFace
      FocusControl = cxLCRight
      ParentColor = False
    end
    object lbLeft: TLabel
      Left = 5
      Top = 80
      Width = 19
      Height = 13
      Caption = 'Left'
      Color = clBtnFace
      ParentColor = False
    end
    object lbTop: TLabel
      Left = 5
      Top = 55
      Width = 18
      Height = 13
      Caption = 'Top'
      Color = clBtnFace
      ParentColor = False
    end
    object cxLCTop: TComboBox
      Left = 88
      Top = 52
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object cxLCLeft: TComboBox
      Left = 88
      Top = 77
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
    object cxLCRight: TComboBox
      Left = 88
      Top = 102
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 5
    end
    object cxLCBottom: TComboBox
      Left = 88
      Top = 127
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 7
    end
    object cxRCTop: TComboBox
      Left = 189
      Top = 52
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
    object cxRCBottom: TComboBox
      Left = 189
      Top = 127
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 8
    end
    object cxRCRight: TComboBox
      Left = 189
      Top = 102
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 6
    end
    object cxRCLeft: TComboBox
      Left = 189
      Top = 77
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 4
    end
    object chkMouseSensors: TCheckBox
      Left = 5
      Top = 17
      Width = 306
      Height = 17
      Caption = 'Enable Mouse Sensors'
      TabOrder = 0
    end
  end
end
