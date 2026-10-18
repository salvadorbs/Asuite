unit Utility.ClipboardController;

{$MODE DelphiUnicode}

interface

uses
  VirtualTrees, Classes;

type
  TClipboardController = class
  private
    FTree: TVirtualStringTree;
    FUseSystemClipboard: Boolean;
  public
    constructor Create(ATree: TVirtualStringTree);
    procedure DoCopy;
    procedure DoCut;
    function  DoPaste: Boolean;
    procedure UpdatePasteEnabled(var Enabled: Boolean; IsActiveTree: Boolean);
    procedure EmptyClipboard;

    // On Windows the controller talks to the system clipboard. A headless
    // environment has none, so the tests switch to the in-memory helpers;
    // on the other platforms those helpers are always used.
    property UseSystemClipboard: Boolean read FUseSystemClipboard
      write FUseSystemClipboard;
  end;

implementation

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Clipbrd, Kernel.Logger, Utility.Misc, VirtualTree.Methods, NodeDataTypes.Base,
  VirtualTrees.ClipBoard, VirtualTrees.Types, VirtualTree.Helper;

constructor TClipboardController.Create(ATree: TVirtualStringTree);
begin
  inherited Create;
  FTree := ATree;
  FUseSystemClipboard := True;
end;

procedure TClipboardController.DoCopy;
begin
  if not Assigned(FTree) then
    Exit;

  TASuiteLogger.Info('Copy nodes into clipboard', []);
  {$IFDEF MSWINDOWS}
  if FUseSystemClipboard then
    FTree.CopyToClipBoard
  else
    FTree.FakeCopyToClipBoard;
  {$ELSE}
  FTree.FakeCopyToClipBoard;
  {$ENDIF}
end;

procedure TClipboardController.DoCut;
begin
  if not Assigned(FTree) then
    Exit;

  TASuiteLogger.Info('Cut nodes into clipboard', []);
  {$IFDEF MSWINDOWS}
  if FUseSystemClipboard then
    FTree.CutToClipBoard
  else
    FTree.FakeCutToClipBoard;
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
  if FUseSystemClipboard then
    Result := FTree.PasteFromClipboard
  else
    Result := FTree.FakePasteFromClipboard;
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
  if FUseSystemClipboard then
    Enabled := IsActiveTree and IsFormatInClipBoard(CF_VIRTUALTREE)
  else
    Enabled := IsActiveTree and (Length(FTree.GetSortedCutCopySet(True)) > 0);
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
