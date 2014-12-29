inherited frmBehaviorPropertyPage: TfrmBehaviorPropertyPage
  object grpAutoExecute: TGroupBox
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
  object grpWindowState: TGroupBox
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
  object grpOnExecute: TGroupBox
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
      170066726D4265686176696F7250726F7065727479506167650001070000000E
      006772704175746F45786563757465010100000001000000070043617074696F
      6E000D0063784175746F4578656375746501010000000200000005004974656D
      73000E0062746E4368616E67654F726465720101000000030000000700436170
      74696F6E000E0067727057696E646F7753746174650101000000040000000700
      43617074696F6E000D00637857696E646F775374617465010100000005000000
      05004974656D73000C006772704F6E4578656375746501010000000600000007
      0043617074696F6E000D006378416374696F6E4F6E4578650101000000070000
      0005004974656D7300}
  end
end
