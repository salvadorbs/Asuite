unit Utility.ClipboardController;

{$MODE DelphiUnicode}

interface

uses
  VirtualTrees, Classes;

type
  TClipboardController = class
  private
    FTree: TVirtualStringTree;
  public
    constructor Create(ATree: TVirtualStringTree);
    procedure DoCopy;
    procedure DoCut;
    function  DoPaste: Boolean;
    procedure UpdatePasteEnabled(var Enabled: Boolean; IsActiveTree: Boolean);
    procedure EmptyClipboard;
  end;

implementation

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Clipbrd, Kernel.Logger, VirtualTree.Methods, NodeDataTypes.Base, Utility.System,
  VirtualTrees.ClipBoard, VirtualTrees.Types, Utility.Misc;

constructor TClipboardController.Create(ATree: TVirtualStringTree);
begin
  inherited Create;
  FTree := ATree;
end;

procedure TClipboardController.DoCopy;
begin
  TASuiteLogger.Info('Copy nodes into clipboard', []);
  {$IFDEF MSWINDOWS}
  FTree.CopyToClipBoard;
  {$ELSE}
  FTree.FakeCopyToClipBoard;
  {$ENDIF}
end;

procedure TClipboardController.DoCut;
begin
  TASuiteLogger.Info('Cut nodes into clipboard', []);
  {$IFDEF MSWINDOWS}
  FTree.CutToClipBoard;
  {$ELSE}
  FTree.FakeCutToClipBoard;
  {$ENDIF}
end;

function TClipboardController.DoPaste: Boolean;
var
  NodeData: TvBaseNodeData;
begin
  TASuiteLogger.Info('Paste clipboard content in ASuite', []);
  Result := False;

  if not Assigned(FTree) then
    Exit;

  NodeData := TVirtualTreeMethods.GetNodeItemData(FTree.GetFirstSelected, FTree);
  if Assigned(NodeData) then
  begin
    if NodeData.IsCategoryItem then
      FTree.DefaultPasteMode := amAddChildLast
    else
      FTree.DefaultPasteMode := amInsertAfter;
  end
  else
    FTree.DefaultPasteMode := amAddChildLast;

  {$IFDEF MSWINDOWS}
  Result := FTree.PasteFromClipboard;
  {$ELSE}
  Result := FTree.FakePasteFromClipboard;
  {$ENDIF}

  if Result then
  begin
    FTree.Expanded[FTree.GetFirstSelected] := True;
    TVirtualTreeMethods.RefreshList(FTree);
  end;
end;

procedure TClipboardController.UpdatePasteEnabled(var Enabled: Boolean; IsActiveTree: Boolean);
begin
  {$IFDEF MSWINDOWS}
  Enabled := IsActiveTree and IsFormatInClipBoard(CF_VIRTUALTREE);
  {$ELSE}
  Enabled := IsActiveTree and (Length(FTree.GetSortedCutCopySet(True)) > 0);
  {$ENDIF}
end;

procedure TClipboardController.EmptyClipboard;
begin
  TASuiteLogger.Enter('Clearing clipboard', Self);
  {$IFDEF MSWINDOWS}
  if IsFormatInClipBoard(CF_VIRTUALTREE) then
  begin
    Windows.OpenClipboard(0);
    try
      Windows.EmptyClipboard;
    finally
      Windows.CloseClipboard;
    end;
  end;
  {$ENDIF}
  Clipbrd.Clipboard.Clear;
end;

end.
