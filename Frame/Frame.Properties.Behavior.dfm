inherited frmBehaviorPropertyPage: TfrmBehaviorPropertyPage
  object grpAutoExecute: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 354
    Height = 78
    Align = alTop
    Caption = 'Autorun'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object cxAutoExecute: TComboBox
      Left = 8
      Top = 17
      Width = 338
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = cxAutoExecuteChange
      Items.Strings = (
        'Never'
        'Always on startup'
        'Startup, only if no previous instances are running'
        'Always on shutdown')
    end
    object btnChangeOrder: TButton
      Left = 102
      Top = 44
      Width = 150
      Height = 21
      Caption = 'Change autorun order'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnChangeOrderClick
    end
  end
  object pnl1: TPanel
    Left = 0
    Top = 84
    Width = 360
    Height = 53
    Align = alTop
    BevelOuter = bvNone
    Locked = True
    TabOrder = 1
    object grpOnExecute: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 173
      Height = 47
      Align = alLeft
      Caption = 'On Execute'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object cxActionOnExe: TComboBox
        Left = 8
        Top = 19
        Width = 158
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Items.Strings = (
          'Default (options)'
          'Just run file'
          'Run and hide BSuite'
          'Run and close BSuite')
      end
    end
    object grpWindowState: TGroupBox
      AlignWithMargins = True
      Left = 182
      Top = 3
      Width = 175
      Height = 47
      Align = alLeft
      Caption = 'Window State'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object cxWindowState: TComboBox
        Left = 8
        Top = 19
        Width = 158
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Items.Strings = (
          'Normal'
          'Minimized'
          'Maximized')
      end
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 248
    LangData = {
      170066726D4265686176696F7250726F7065727479506167650001080000000E
      006772704175746F45786563757465010100000001000000070043617074696F
      6E000D0063784175746F4578656375746501010000000200000005004974656D
      73000E0062746E4368616E67654F726465720101000000030000000700436170
      74696F6E000400706E6C3100000C006772704F6E457865637574650101000000
      04000000070043617074696F6E000D006378416374696F6E4F6E457865010100
      00000500000005004974656D73000E0067727057696E646F7753746174650101
      00000006000000070043617074696F6E000D00637857696E646F775374617465
      01010000000700000005004974656D7300}
  end
end
