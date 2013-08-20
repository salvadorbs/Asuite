inherited frmBehaviorPropertyPage: TfrmBehaviorPropertyPage
  object grpAutoExecute: TGroupBox [0]
    Left = 3
    Top = 3
    Width = 354
    Height = 53
    Caption = 'Autorun'
    TabOrder = 0
    object cxAutoExecute: TComboBox
      Left = 8
      Top = 17
      Width = 253
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cxAutoExecuteChange
      Items.Strings = (
        'Never'
        'Always on startup'
        'Startup, only if no previous instances are running'
        'Always on shutdown')
    end
    object btnChangeOrder: TButton
      Left = 267
      Top = 17
      Width = 79
      Height = 21
      Caption = 'Change order'
      TabOrder = 1
      OnClick = btnChangeOrderClick
    end
  end
  object grpWindowState: TGroupBox [1]
    Left = 191
    Top = 56
    Width = 166
    Height = 53
    Caption = 'Window State'
    TabOrder = 1
    object cxWindowState: TComboBox
      Left = 8
      Top = 19
      Width = 150
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'Normal'
        'Minimized'
        'Maximized')
    end
  end
  object grpOnExecute: TGroupBox [2]
    Left = 3
    Top = 56
    Width = 182
    Height = 53
    Caption = 'On Execute'
    TabOrder = 2
    object cxActionOnExe: TComboBox
      Left = 8
      Top = 19
      Width = 165
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'Default (options)'
        'Just run file'
        'Run and hide ASuite'
        'Run and close ASuite')
    end
  end
  object DKLanguageController1: TDKLanguageController
    Left = 8
    Top = 248
    LangData = {
      170066726D4265686176696F7250726F7065727479506167650001080000000B
      004F70656E4469616C6F673100000E006772704175746F457865637574650101
      00000001000000070043617074696F6E000D0063784175746F45786563757465
      01010000000200000005004974656D73000E0062746E4368616E67654F726465
      72010100000003000000070043617074696F6E000E0067727057696E646F7753
      74617465010100000004000000070043617074696F6E000D00637857696E646F
      77537461746501010000000500000005004974656D73000C006772704F6E4578
      6563757465010100000006000000070043617074696F6E000D00637841637469
      6F6E4F6E45786501010000000700000005004974656D7300}
  end
end
