inherited frmBehaviorPropertyPage: TfrmBehaviorPropertyPage
  Width = 360
  ExplicitWidth = 360
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
    end
  end
  object grpWindowState: TGroupBox [1]
    Left = 215
    Top = 56
    Width = 142
    Height = 53
    Caption = 'Window State'
    TabOrder = 1
    object cxWindowState: TComboBox
      Left = 8
      Top = 19
      Width = 126
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
    Width = 207
    Height = 53
    Caption = 'On Execute'
    TabOrder = 2
    object cxActionOnExe: TComboBox
      Left = 8
      Top = 17
      Width = 191
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
end
