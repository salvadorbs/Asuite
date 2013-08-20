inherited frmSensorsOptionsPage: TfrmSensorsOptionsPage
  object gbMouse: TGroupBox [0]
    Left = 3
    Top = 3
    Width = 320
    Height = 158
    Caption = 'Mouse sensors'
    TabOrder = 0
    object lbSide: TLabel
      Left = 8
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
      Left = 8
      Top = 130
      Width = 34
      Height = 13
      Caption = 'Bottom'
      Color = clBtnFace
      FocusControl = cxRCBottom
      ParentColor = False
    end
    object lbRight: TLabel
      Left = 8
      Top = 105
      Width = 25
      Height = 13
      Caption = 'Right'
      Color = clBtnFace
      FocusControl = cxLCRight
      ParentColor = False
    end
    object lbLeft: TLabel
      Left = 8
      Top = 80
      Width = 19
      Height = 13
      Caption = 'Left'
      Color = clBtnFace
      ParentColor = False
    end
    object lbTop: TLabel
      Left = 8
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
      Items.Strings = (
        'Disabled'
        'Show window'
        'Show default menu'
        'Show classic menu')
    end
    object cxLCLeft: TComboBox
      Left = 88
      Top = 77
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'Disabled'
        'Show window'
        'Show default menu'
        'Show classic menu')
    end
    object cxLCRight: TComboBox
      Left = 88
      Top = 102
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      Items.Strings = (
        'Disabled'
        'Show window'
        'Show default menu'
        'Show classic menu')
    end
    object cxLCBottom: TComboBox
      Left = 88
      Top = 127
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 7
      Items.Strings = (
        'Disabled'
        'Show window'
        'Show default menu'
        'Show classic menu')
    end
    object cxRCTop: TComboBox
      Left = 189
      Top = 52
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'Disabled'
        'Show window'
        'Show default menu'
        'Show classic menu')
    end
    object cxRCBottom: TComboBox
      Left = 189
      Top = 127
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 8
      Items.Strings = (
        'Disabled'
        'Show window'
        'Show default menu'
        'Show classic menu')
    end
    object cxRCRight: TComboBox
      Left = 189
      Top = 102
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 6
      Items.Strings = (
        'Disabled'
        'Show window'
        'Show default menu'
        'Show classic menu')
    end
    object cxRCLeft: TComboBox
      Left = 189
      Top = 77
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      Items.Strings = (
        'Disabled'
        'Show window'
        'Show default menu'
        'Show classic menu')
    end
    object cbMouseSensors: TCheckBox
      Left = 8
      Top = 17
      Width = 306
      Height = 17
      Caption = 'Enable Mouse Sensors'
      TabOrder = 0
      OnClick = cbMouseSensorsClick
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 248
    LangData = {
      150066726D53656E736F72734F7074696F6E73506167650001120000000B004F
      70656E4469616C6F67310000070067624D6F7573650101000000010000000700
      43617074696F6E0006006C625369646501010000000200000007004361707469
      6F6E000B006C624C656674436C69636B01010000000300000007004361707469
      6F6E000C006C625269676874436C69636B010100000004000000070043617074
      696F6E0008006C62426F74746F6D010100000005000000070043617074696F6E
      0007006C625269676874010100000006000000070043617074696F6E0006006C
      624C656674010100000007000000070043617074696F6E0005006C62546F7001
      0100000008000000070043617074696F6E00070063784C43546F700101000000
      0A00000005004974656D7300080063784C434C65667401010000000B00000005
      004974656D7300090063784C43526967687401010000000C0000000500497465
      6D73000A0063784C43426F74746F6D01010000000D00000005004974656D7300
      070063785243546F7001010000000E00000005004974656D73000A0063785243
      426F74746F6D01010000000F00000005004974656D7300090063785243526967
      687401010000001000000005004974656D73000800637852434C656674010100
      00001100000005004974656D73000E0063624D6F75736553656E736F72730101
      00000009000000070043617074696F6E00}
  end
end
