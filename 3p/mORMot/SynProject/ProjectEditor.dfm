object FrameEditor: TFrameEditor
  Left = 0
  Top = 0
  Width = 904
  Height = 238
  TabOrder = 0
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 904
    Height = 238
    Align = alClient
    AutoSize = True
    TabOrder = 0
    object Memo: TMemoEx
      Left = 121
      Top = 35
      Width = 782
      Height = 202
      Cursor = crIBeam
      TabOrder = 0
      GutterWidth = 0
      RightMarginColor = clSilver
      Completion.Separator = '='
      Completion.ItemHeight = 13
      Completion.Interval = 800
      Completion.ListBoxStyle = lbStandard
      Completion.CaretChar = '|'
      Completion.CRLF = '/n'
      TabStops = '8'
      CursorBeyondEOL = True
      SelForeColor = clHighlightText
      SelBackColor = clHighlight
      OnKeyDown = MemoKeyDown
      OnChange = MemoChange
      OnMouseOver = MemoMouseOver
      OnSetCaretPos = MemoSetCaretPos
      Align = alClient
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentColor = False
      PopupMenu = EditorPopup
      TabStop = True
      UseDockManager = False
      WordWrap = False
    end
    object ToolBar: TToolBar
      Left = 1
      Top = 1
      Width = 902
      Height = 34
      ButtonHeight = 30
      ButtonWidth = 31
      DisabledImages = ImageListDisabled
      Images = ImageListEnabled
      TabOrder = 1
      object BtnHistoryBack: TToolButton
        Left = 0
        Top = 2
        Enabled = False
        ImageIndex = 5
        OnClick = BtnHistoryBackClick
      end
      object BtnHistoryNext: TToolButton
        Left = 31
        Top = 2
        Enabled = False
        ImageIndex = 6
        OnClick = BtnHistoryNextClick
      end
      object BtnTextAll: TToolButton
        Left = 62
        Top = 2
        Hint = 'Whole File edition'
        ImageIndex = 12
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton1: TToolButton
        Left = 93
        Top = 2
        Width = 8
        ImageIndex = 3
        Style = tbsSeparator
      end
      object BtnReadOnly: TToolButton
        Left = 101
        Top = 2
        Hint = 'Read Only'
        ImageIndex = 0
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnReadOnlyClick
      end
      object BtnSave: TToolButton
        Left = 132
        Top = 2
        Hint = 'Save (Ctrl+S)'
        ImageIndex = 3
        ParentShowHint = False
        ShowHint = True
      end
      object BtnHistory: TToolButton
        Left = 163
        Top = 2
        Hint = 'File History'
        DropdownMenu = PopupMenuBtnHistory
        ImageIndex = 21
        ParentShowHint = False
        ShowHint = True
        Visible = False
      end
      object BtnWizard: TToolButton
        Left = 194
        Top = 2
        Hint = 'Documentation Wizard'
        ImageIndex = 23
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnWizardClick
      end
      object BtnDocument: TToolButton
        Left = 225
        Top = 2
        Hint = 'Create Word Document'
        ImageIndex = 15
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnLinkSectionClick
      end
      object ToolButton2: TToolButton
        Left = 256
        Top = 2
        Width = 8
        ImageIndex = 4
        Style = tbsSeparator
      end
      object BtnUndo: TToolButton
        Left = 264
        Top = 2
        Hint = 'Undo'
        ImageIndex = 10
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnUndoClick
      end
      object BtnWordWrap: TToolButton
        Left = 295
        Top = 2
        Hint = 'Word Wrap'
        ImageIndex = 2
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnWordWrapClick
      end
      object BtnSpellCheck: TToolButton
        Left = 326
        Top = 2
        Hint = 'Spell Check selection (F7)'
        ImageIndex = 26
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnSpellCheckClick
      end
      object ToolButton3: TToolButton
        Left = 357
        Top = 2
        Width = 8
        ImageIndex = 3
        Style = tbsSeparator
      end
      object BtnBold: TToolButton
        Left = 365
        Top = 2
        Hint = 'Selected text to Bold (Ctrl+B)'
        ImageIndex = 7
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnBoldItalicUnderlineClick
      end
      object BtnItalic: TToolButton
        Tag = 1
        Left = 396
        Top = 2
        Hint = 'Selected text to Italic (Ctrl+I)'
        ImageIndex = 8
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnBoldItalicUnderlineClick
      end
      object BtnUnderline: TToolButton
        Tag = 2
        Left = 427
        Top = 2
        Hint = 'Selected text to Underline (Ctrl+U)'
        ImageIndex = 9
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnBoldItalicUnderlineClick
      end
      object BtnFixedFont: TToolButton
        Tag = 3
        Left = 458
        Top = 2
        Hint = 'Selected text to FixedFont (Ctrl+0)'
        ImageIndex = 16
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnBoldItalicUnderlineClick
      end
      object BtnMarkProgram: TToolButton
        Left = 489
        Top = 2
        Hint = 'Mark Selected lines as program code'
        DropdownMenu = PopupMenuProgram
        ImageIndex = 17
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnMarkProgramClick
      end
      object BtnLinkSection: TToolButton
        Left = 520
        Top = 2
        Hint = 'Link to Section'
        ImageIndex = 11
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnLinkSectionClick
      end
      object BtnLinkPeople: TToolButton
        Left = 551
        Top = 2
        Hint = 'Link to People'
        ImageIndex = 13
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnLinkSectionClick
      end
      object BtnLinkPicture: TToolButton
        Left = 582
        Top = 2
        Hint = 'Link to Picture'
        ImageIndex = 14
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnLinkSectionClick
      end
      object BtnLinkProgram: TToolButton
        Left = 613
        Top = 2
        Hint = 'Link to an Object (.pas only)'
        ImageIndex = 28
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnLinkProgramClick
      end
      object ToolButton4: TToolButton
        Left = 644
        Top = 2
        Width = 8
        ImageIndex = 19
        Style = tbsSeparator
      end
      object BtnAddPicture: TToolButton
        Left = 652
        Top = 2
        Hint = 'Add Picture'
        ImageIndex = 19
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnLinkSectionClick
      end
      object BtnAddGraph: TToolButton
        Left = 683
        Top = 2
        Hint = 'Add/Edit a Graph'
        ImageIndex = 25
        ParentShowHint = False
        ShowHint = True
        Visible = False
        OnClick = BtnAddGraphClick
      end
      object BtnAutoSections: TToolButton
        Left = 714
        Top = 2
        Hint = 'Create missing sections for current Document'
        ImageIndex = 18
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnAutoSectionsClick
      end
      object BtnReleaseDocument: TToolButton
        Left = 745
        Top = 2
        Hint = 'Increment the the document history'
        ImageIndex = 22
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnReleaseDocumentClick
      end
      object BtnAddTracker: TToolButton
        Left = 776
        Top = 2
        Hint = 'Add a new Tracker entry (F1)'
        ImageIndex = 20
        ParentShowHint = False
        ShowHint = True
        Visible = False
      end
      object BtnImportTracker: TToolButton
        Left = 807
        Top = 2
        Hint = 'Import entries from PVCS'
        Caption = 'BtnImportTracker'
        ImageIndex = 24
        ParentShowHint = False
        ShowHint = True
        Visible = False
      end
      object ToolButton5: TToolButton
        Left = 838
        Top = 2
        Width = 8
        Caption = 'ToolButton5'
        ImageIndex = 28
        Style = tbsSeparator
      end
      object BtnAbout: TToolButton
        Left = 846
        Top = 2
        Hint = 'About'
        ImageIndex = 27
        ParentShowHint = False
        ShowHint = True
        OnClick = BtnAboutClick
      end
    end
    object Sections: TListBox
      Left = 1
      Top = 35
      Width = 120
      Height = 202
      Align = alLeft
      ItemHeight = 13
      TabOrder = 2
      Visible = False
      OnClick = SectionsClick
      OnMouseDown = SectionsMouseDown
    end
  end
  object ImageListEnabled: TImageList
    Height = 24
    Width = 24
    Left = 168
    Top = 168
  end
  object ImageListDisabled: TImageList
    Height = 24
    Width = 24
    Left = 256
    Top = 168
  end
  object PopupMenuLink: TPopupMenu
    Images = ImageList16
    Left = 536
    Top = 40
  end
  object FindDialog: TFindDialog
    Options = [frDown, frHideWholeWord, frHideUpDown]
    OnFind = FindDialogFind
    Left = 128
    Top = 40
  end
  object PopupMenuProgram: TPopupMenu
    Images = ImageList16
    Left = 496
    Top = 40
    object PopupMenuProgramDelphi: TMenuItem
      Caption = 'Delphi/Pascal'
      Hint = '!'
      ImageIndex = 17
      ShortCut = 113
      OnClick = BtnMarkProgramClick
    end
    object PopupMenuProgramModula2: TMenuItem
      Caption = 'Modula2'
      Hint = #181
      ImageIndex = 17
      OnClick = BtnMarkProgramClick
    end
    object PopupMenuProgramC: TMenuItem
      Caption = 'C/C++'
      Hint = '&'
      ImageIndex = 17
      OnClick = BtnMarkProgramClick
    end
    object PopupMenuProgramCSharp: TMenuItem
      Caption = 'C#'
      Hint = '#'
      ImageIndex = 17
      OnClick = BtnMarkProgramClick
    end
    object PopupMenuProgramINI: TMenuItem
      Caption = 'INI/TXT'
      Hint = '$'
      ImageIndex = 17
      OnClick = BtnMarkProgramClick
    end
    object PopupMenuProgramXML: TMenuItem
      Caption = 'XML/HTML'
      Hint = '$$'
      ImageIndex = 17
      OnClick = BtnMarkProgramClick
    end
    object PopupMenuProgramDFM: TMenuItem
      Caption = 'DFM'
      Hint = '!$'
      ImageIndex = 17
      OnClick = BtnMarkProgramClick
    end
    object PopupMenuProgramComment: TMenuItem
      Caption = '; comment'
      Hint = ';'
      ImageIndex = 17
      OnClick = BtnMarkProgramClick
    end
  end
  object PopupMenuBtnHistory: TPopupMenu
    Left = 168
    Top = 40
  end
  object EditorPopup: TPopupMenu
    Images = ImageList16
    OnPopup = EditorPopupPopup
    Left = 464
    Top = 168
    object EditorPopupCopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = EditorPopupCopyClick
    end
    object EditorPopupCopyAs: TMenuItem
      Caption = 'Copy as'
      object EditorPopupCopyAsHtml: TMenuItem
        Caption = 'HTML'
        OnClick = EditorPopupCopyAsHtmlClick
      end
      object EditorPopupCopyAsBBCode: TMenuItem
        Caption = 'BBCODE'
        OnClick = EditorPopupCopyAsBBCodeClick
      end
    end
    object EditorPopupPaste: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = EditorPopupPasteClick
    end
    object EditorPopupCut: TMenuItem
      Caption = 'Cut'
      ShortCut = 16472
      OnClick = EditorPopupCutClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object EditorPopupUndo: TMenuItem
      Caption = 'Undo'
      ImageIndex = 10
      ShortCut = 16474
      OnClick = BtnUndoClick
    end
    object EditorPopupWordWrap: TMenuItem
      Caption = 'Word Wrap'
      ImageIndex = 2
      OnClick = BtnWordWrapClick
    end
    object EditorPopupSpellCheck: TMenuItem
      Caption = 'Spell check'
      ImageIndex = 26
      ShortCut = 118
      OnClick = BtnSpellCheckClick
    end
  end
  object ImageList16: TImageList
    Left = 344
    Top = 168
  end
end
