object frmPropertyGroup: TfrmPropertyGroup
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Properties'
  ClientHeight = 315
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 235
    Top = 279
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 319
    Top = 279
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 9
    Width = 385
    Height = 264
    ActivePage = tsInfo1
    TabOrder = 0
    object tsInfo1: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbPathExe: TLabel
        Left = 8
        Top = 48
        Width = 161
        Height = 13
        Caption = 'Executable/folder/web page path'
      end
      object lbName: TLabel
        Left = 8
        Top = 8
        Width = 41
        Height = 13
        AutoSize = False
        Caption = 'Name'
      end
      object btnAdd: TButton
        Left = 161
        Top = 210
        Width = 65
        Height = 17
        Caption = 'Add'
        TabOrder = 6
        OnClick = btnAddClick
      end
      object btnReplace: TButton
        Left = 233
        Top = 210
        Width = 65
        Height = 17
        Caption = 'Replace'
        TabOrder = 7
        OnClick = btnReplaceClick
      end
      object btnDelete: TButton
        Left = 304
        Top = 210
        Width = 65
        Height = 17
        Caption = 'Delete'
        TabOrder = 8
        OnClick = btnDeleteClick
      end
      object edtName: TEdit
        Left = 8
        Top = 24
        Width = 249
        Height = 21
        TabOrder = 0
      end
      object edtPathExe: TEdit
        Left = 9
        Top = 183
        Width = 289
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Text = '$ASuite\'
      end
      object btnBrowseExe: TButton
        Left = 304
        Top = 183
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 5
        OnClick = Browse
      end
      object bbtnDown: TBitBtn
        Left = 342
        Top = 127
        Width = 27
        Height = 25
        TabOrder = 3
        OnClick = bbtnDownClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF638E78638E78FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9AACA32B
          8A592B8A599AACA3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFD2D5D3167A4674D1A275D3A3167B46D2D5D3FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF21784B5DBD8B4C
          D18D4ED5905FC39021784CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF55886D4EA87A6DD6A012C46914CB6E6CE0A542A57156886EFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9AACA32D8B5A94DBB731C37911
          C06712C56A1FC97378DBA82588559AACA3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          D2D5D315784490D3B165CC983BC27D0FB9620FBC6510BD6532C57B71CE9F1278
          43D2D5D3FFFFFFFFFFFFFFFFFFFFFFFF21774A75BD9783D1A952C2894DC3871E
          B6690DB45F0DB45F0DB35E4EC58955B58221784BFFFFFFFFFFFFFFFFFF55876D
          51A17A9BD6B85DBE8D58C08A51C08742BC7E0CAB5A0BAB590AAA590CA95868C7
          9738986556886EFFFFFF9AACA32D8759AADBC2AFDEC6ADDDC5AADCC256BD884D
          BA821FAA6308A2537FCEA57ECDA47ECCA478C79F1F824E9AACA33B7F5D04733A
          04733A04733A0B753EADDDC559BC8950B88234AD6F069A4E7ECAA30B753E0473
          3A04733A04733A3B7F5DFFFFFFFFFFFFFFFFFFFFFFFF04733AB0DEC75DBD8C54
          B98547B47C069A4E7ECAA304733AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF04733AB1DFC75FBE8D55BA8641B278069A4E7ECAA304733AFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF04733AB1DFC85FBE8D55
          BA8637AE71069A4E7ECAA304733AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF04733AB0DEC7ABDCC3A6DABF91D2B17ECAA37ECAA304733AFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1A774704733A04733A04
          733A04733A04733A04733A1A7747FFFFFFFFFFFFFFFFFFFFFFFF}
      end
      object bbtnUp: TBitBtn
        Left = 342
        Top = 96
        Width = 27
        Height = 25
        TabOrder = 2
        OnClick = bbtnUpClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFF1A774704733A04733A04733A04733A04733A04733A1A7748FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF04733AA5DFC181D5AA81
          D7AB82D9AC82DAAD82D9AD04733AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF04733AA9E2C516B6650FBA6310BE6611C16783DEB004733AFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF04733AADE4C81FBD6D10
          BF6612C56A13C96D84E3B304733AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF04733AB0E5CA31C37811C06713C86C15CF7185E5B404733AFFFF
          FFFFFFFFFFFFFFFFFFFF3B7F5D04733A04733A04733A0B773FB2E6CB4BC98910
          BD6511C36912C76B84E1B20C794104733A04733A04733A3B7F5D9AACA32E895B
          B5E0CABCE5D0B8E5CEB3E5CB63CD9719BA680FBC6410BE6583DCAF82DBAE82D9
          AC7BD3A61F844F9AACA3FFFFFF55876D58A67EB0DFC777CBA06CCA9A63CA9643
          C1810DB35E0DB55F0DB45F0FB25F6ACE9B399A6756886EFFFFFFFFFFFFFFFFFF
          21774A7FC29F97D5B569C59660C59156C38B1DB0660BAC590BAB594DC18555B2
          8221784BFFFFFFFFFFFFFFFFFFFFFFFFD2D5D31578449CD4B77AC9A05ABE8B51
          BC8638B47408A3532CAF6C6EC397127742D2D5D3FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF9AACA32E885AA0D7BB5FBD8C4DB68044B37A1AA25C71C49A2484539AAC
          A3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF55876D4EA37891D2B048
          B47D3FB1766AC2953E996B56886EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF21774A72BB9472C59A6AC2955BB18421774AFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2D5D316794587
          CAA77EC7A2167945D2D5D3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF9AACA32D885A2D88599AACA3FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF63
          8D78638D78FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
      object lxPathExe: TListBox
        Left = 8
        Top = 65
        Width = 329
        Height = 112
        Style = lbOwnerDrawFixed
        ItemHeight = 13
        TabOrder = 1
        OnClick = lxPathExeClick
        OnDrawItem = lxPathExeDrawItem
      end
    end
    object tsInfo2: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbScheduler: TLabel
        Left = 8
        Top = 136
        Width = 47
        Height = 13
        Caption = 'Scheduler'
      end
      object lbAutoExecute: TLabel
        Left = 8
        Top = 48
        Width = 62
        Height = 13
        Caption = 'Autoexecute'
      end
      object lbWindowState: TLabel
        Left = 266
        Top = 135
        Width = 66
        Height = 13
        Caption = 'Window state'
      end
      object lbActionOnExe: TLabel
        Left = 215
        Top = 89
        Width = 64
        Height = 13
        Caption = 'On execution'
      end
      object lbPathIcon: TLabel
        Left = 8
        Top = 8
        Width = 135
        Height = 13
        Caption = ' Custom icon path (optional)'
      end
      object cxScheduler: TComboBox
        Left = 8
        Top = 152
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 8
        OnChange = cxSchedulerChange
      end
      object dtpSchDate: TDateTimePicker
        Left = 8
        Top = 176
        Width = 129
        Height = 21
        Date = 39092.942071932870000000
        Time = 39092.942071932870000000
        TabOrder = 10
      end
      object dtpSchTime: TDateTimePicker
        Left = 8
        Top = 200
        Width = 129
        Height = 21
        Date = 39092.942939814810000000
        Time = 39092.942939814810000000
        DateMode = dmUpDown
        Kind = dtkTime
        TabOrder = 12
      end
      object cbDontInsertMRU: TCheckBox
        Left = 143
        Top = 203
        Width = 231
        Height = 22
        Caption = 'Don'#39't insert this software group in MRU'
        TabOrder = 13
        WordWrap = True
      end
      object cxHotkey1: TComboBox
        Left = 8
        Top = 108
        Width = 121
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 5
        Items.Strings = (
          'Alt'
          'Crtl'
          'Shift'
          'Crtl + Alt'
          'Shift + Alt'
          'Shift + Crtl'
          'Shift + Crtl + Alt'
          'WinKey'
          'WinKey + Alt'
          'WinKey + Crtl'
          'WinKey + Shift'
          'WinKey + Crtl + Alt'
          'WinKey + Shift + Alt'
          'WinKey + Shift + Crtl'
          'WinKey + Shift + Crtl + Alt')
      end
      object cbHotKey: TCheckBox
        Left = 8
        Top = 88
        Width = 201
        Height = 17
        Caption = 'Active hotkey'
        TabOrder = 4
        OnClick = cbHotKeyClick
      end
      object cxHotKey2: TComboBox
        Left = 136
        Top = 108
        Width = 49
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        Items.Strings = (
          'A'
          'B'
          'C'
          'D'
          'E'
          'F'
          'G'
          'H'
          'I'
          'J'
          'K'
          'L'
          'M'
          'N'
          'O'
          'P'
          'Q'
          'R'
          'S'
          'T'
          'U'
          'V'
          'W'
          'X'
          'Y'
          'Z'
          'F1'
          'F2'
          'F3'
          'F4'
          'F5'
          'F6'
          'F7'
          'F8'
          'F9'
          'F10'
          'F11'
          'F12'
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9'
          '0')
      end
      object cxAutoExecute: TComboBox
        Left = 8
        Top = 64
        Width = 233
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 2
        OnChange = cxAutoExecuteChange
      end
      object cxWindowState: TComboBox
        Left = 267
        Top = 152
        Width = 102
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 9
      end
      object cxActionOnExe: TComboBox
        Left = 216
        Top = 108
        Width = 153
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 7
      end
      object edtPathIcon: TEdit
        Left = 8
        Top = 24
        Width = 289
        Height = 21
        TabOrder = 0
      end
      object btnBrowseIcon: TButton
        Left = 304
        Top = 24
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 1
        OnClick = Browse
      end
      object cbHideSoftware: TCheckBox
        Left = 143
        Top = 175
        Width = 231
        Height = 25
        Caption = 'Hide this software from menu'
        TabOrder = 11
        WordWrap = True
      end
      object btnChangeOrder: TButton
        Left = 267
        Top = 64
        Width = 102
        Height = 21
        Caption = 'Change order'
        TabOrder = 3
        OnClick = btnChangeOrderClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 72
    Top = 271
  end
end
