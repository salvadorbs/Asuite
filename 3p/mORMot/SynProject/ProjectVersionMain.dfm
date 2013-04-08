object MainForm: TMainForm
  Left = 244
  Top = 158
  Width = 694
  Height = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterPanel: TSplitter
    Left = 305
    Top = 42
    Height = 296
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 678
    Height = 42
    AutoSize = True
    ButtonHeight = 36
    ButtonWidth = 46
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    Images = ImageList
    ShowCaptions = True
    TabOrder = 0
    object BtnReleaseOpen: TToolButton
      Left = 0
      Top = 2
      Hint = 'Open another Release .dvs file'
      Caption = 'Open'
      DropdownMenu = PopupMenuOpen
      ImageIndex = 7
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnReleaseOpenClick
    end
    object BtnReleaseNew: TToolButton
      Left = 46
      Top = 2
      Hint = 'Create a new Release (.dvs and associated directories and files)'
      Caption = 'New'
      ImageIndex = 8
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnReleaseNewClick
    end
    object BtnReleaseSettings: TToolButton
      Left = 92
      Top = 2
      Hint = 'Release Settings (F9)'
      Caption = 'Settings'
      ImageIndex = 0
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnReleaseSettingsClick
    end
    object ToolButton1: TToolButton
      Left = 138
      Top = 2
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object BtnSCR: TToolButton
      Left = 146
      Top = 2
      Hint = 'SCR Editor (F1)'
      Caption = 'SCR'
      ImageIndex = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnSCRClick
    end
    object BtnSCRAdd: TToolButton
      Left = 192
      Top = 2
      Hint = 'Add a new Tracker entry to the SCR file'
      Caption = 'Entry'
      ImageIndex = 6
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnSCRAddClick
    end
    object ToolButton5: TToolButton
      Left = 238
      Top = 2
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object BtnManual: TToolButton
      Left = 246
      Top = 2
      Hint = 'Software Manual Editor'
      Caption = 'Manual'
      ImageIndex = 10
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnManualClick
    end
    object ToolButton3: TToolButton
      Left = 292
      Top = 2
      Width = 8
      ImageIndex = 5
      Style = tbsSeparator
    end
    object BtnPRO: TToolButton
      Left = 300
      Top = 2
      Hint = 'Release Documentation Editor (F2)'
      Caption = 'Docs'
      ImageIndex = 3
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnPROClick
    end
    object BtnPROWizard: TToolButton
      Left = 346
      Top = 2
      Hint = 'Release Documentation Wizard'
      Caption = 'Wizard'
      ImageIndex = 5
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnPROWizardClick
    end
    object ToolButton2: TToolButton
      Left = 392
      Top = 2
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object BtnCommit: TToolButton
      Left = 400
      Top = 2
      Hint = 'Commit source modifications (F3)'
      Caption = 'Commit'
      ImageIndex = 4
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnCommitClick
    end
    object BtnBackup: TToolButton
      Left = 446
      Top = 2
      Hint = 'Incremental Backup (F4)'
      Caption = 'Backup'
      ImageIndex = 1
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnBackupClick
    end
    object BtnBackupOpen: TToolButton
      Left = 492
      Top = 2
      Hint = 'View a backup content'
      Caption = 'GetBack'
      DropdownMenu = PopupMenuBackup
      ImageIndex = 11
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnBackupOpenClick
    end
    object ToolButton4: TToolButton
      Left = 538
      Top = 2
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 12
      Style = tbsSeparator
    end
    object BtnAbout: TToolButton
      Left = 546
      Top = 2
      Caption = 'About'
      ImageIndex = 9
      OnClick = BtnAboutClick
    end
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 42
    Width = 305
    Height = 296
    Align = alLeft
    TabOrder = 1
  end
  object PanelRight: TPanel
    Left = 308
    Top = 42
    Width = 370
    Height = 296
    Align = alClient
    TabOrder = 2
  end
  object ImageList: TImageList
    Left = 32
    Top = 181
  end
  object PopupMenuBackup: TPopupMenu
    Left = 500
    Top = 50
  end
  object PopupMenuOpen: TPopupMenu
    Left = 8
    Top = 50
  end
end
