object Form1: TForm1
  Left = 213
  Top = 114
  Width = 414
  Height = 534
  Caption = 'Just testing...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnMouseDown = FormMouseDown
  OnMouseUp = FormMouseUp
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 140
    Width = 129
    Height = 65
    Caption = 'GroupBox1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 10
    object RadioButton2: TRadioButton
      Left = 20
      Top = 36
      Width = 85
      Height = 17
      Alignment = taLeftJustify
      Caption = 'RadioButton2'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton1: TRadioButton
      Left = 20
      Top = 20
      Width = 89
      Height = 17
      Caption = 'RadioButton1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 1
      OnClick = RadioButton1Click
    end
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 12
    Width = 133
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ImeName = '???????'
    ItemHeight = 13
    ParentFont = False
    TabOrder = 0
    Text = 'sample text'
    OnChange = ComboBox1Change
    OnClick = ComboBox1Click
    Items.Strings = (
      '11'
      '22'
      '44'
      '33')
  end
  object Edit1: TEdit
    Left = 276
    Top = 12
    Width = 121
    Height = 21
    ImeName = '???????'
    TabOrder = 1
    Text = 'New item name'
    OnDblClick = Edit1DblClick
  end
  object Button1: TButton
    Left = 184
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Add'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 184
    Top = 44
    Width = 75
    Height = 25
    Caption = 'Clear'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button3Click
  end
  object ListBox1: TListBox
    Left = 184
    Top = 76
    Width = 217
    Height = 129
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ImeName = '???????'
    ItemHeight = 13
    Items.Strings = (
      '444'
      '111'
      '333'
      '222')
    ParentFont = False
    TabOrder = 4
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
  end
  object Memo1: TMemo
    Left = 4
    Top = 216
    Width = 397
    Height = 285
    ImeName = '???????'
    Lines.Strings = (
      'asd'
      'zxc')
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object Button2: TButton
    Left = 276
    Top = 44
    Width = 53
    Height = 25
    Caption = 'GetText'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 344
    Top = 44
    Width = 53
    Height = 25
    Caption = 'SetText'
    TabOrder = 7
    OnClick = Button4Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 76
    Width = 73
    Height = 17
    Alignment = taLeftJustify
    AllowGrayed = True
    Caption = 'CheckBox1'
    State = cbGrayed
    TabOrder = 8
  end
  object Button6: TButton
    Left = 96
    Top = 92
    Width = 45
    Height = 25
    Caption = 'Grayed'
    TabOrder = 9
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 148
    Top = 12
    Width = 29
    Height = 25
    Caption = 'DD'
    TabOrder = 11
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 80
    Top = 44
    Width = 61
    Height = 21
    Caption = 'Disabled'
    Enabled = False
    TabOrder = 12
  end
  object Button9: TButton
    Left = 144
    Top = 180
    Width = 33
    Height = 25
    Caption = 'Sh/H'
    TabOrder = 13
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 8
    Top = 44
    Width = 69
    Height = 21
    Caption = 'Invisible!!!'
    TabOrder = 14
    Visible = False
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 96
    Width = 77
    Height = 17
    Caption = 'CheckBox2'
    Checked = True
    State = cbChecked
    TabOrder = 15
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 116
    Width = 73
    Height = 17
    Alignment = taLeftJustify
    Caption = 'CheckBox3'
    TabOrder = 16
  end
  object ComboBox2: TComboBox
    Left = 148
    Top = 44
    Width = 25
    Height = 129
    Style = csSimple
    ImeName = #1056#1091#1089#1089#1082#1072#1103
    ItemHeight = 13
    TabOrder = 17
    Text = '10'
    Items.Strings = (
      '5'
      '4'
      '3'
      '2'
      '1')
  end
end
