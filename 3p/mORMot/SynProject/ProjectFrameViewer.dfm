object FrameViewer: TFrameViewer
  Left = 0
  Top = 0
  Width = 318
  Height = 238
  TabOrder = 0
  object MemoEx: TMemoEx
    Left = 0
    Top = 0
    Width = 318
    Height = 238
    Cursor = crIBeam
    TabOrder = 0
    GutterWidth = 0
    RightMarginColor = clSilver
    ReadOnly = True
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
    OnKeyDown = MemoExKeyDown
    Align = alClient
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentColor = False
    TabStop = True
    UseDockManager = False
    WordWrap = False
  end
  object FindDialog: TFindDialog
    Options = [frDown, frHideWholeWord, frHideUpDown]
    OnFind = FindDialogFind
    Left = 72
    Top = 72
  end
end
