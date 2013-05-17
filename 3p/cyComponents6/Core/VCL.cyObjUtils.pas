{   Unit VCL.cyObjUtils

    Description:
    Unit with functions for classes, controls handling

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit VCL.cyObjUtils;

{$I cyCompilerDefines.inc}

interface

uses Classes, Types, Windows, Graphics, Forms, ComCtrls, Controls, RichEdit, SysUtils, cyStrUtils;

type
  TStringsSortType = (stNone, stStringSensitive, stStringInsensitive, stExtended);
  TStringsValueKind = (skStringSensitive, skStringInsensitive, skExtended);

  TcyLocateOption = (lCaseInsensitive, lPartialKey);
  TcyLocateOptions = set of TcyLocateOption;


  // TStrings functions :
  function StringsLocate(aList : TStrings; Value: String; Options: TcyLocateOptions): Integer; overload;
  function StringsLocate(aList: TStrings; Value: String; ValueKind: TStringsValueKind): Integer; overload;
  function StringsAdd(aList: TStrings; Value: String; Unique: Boolean; SortType: TStringsSortType): Integer;
  procedure StringsReplace(aList: TStrings; OldStr: String; NewStr: String; ValueKind: TStringsValueKind);
  procedure StringsSort(aList: TStrings; SortType: TStringsSortType);

  // TTreeNode functions:
  function TreeNodeLocate(ParentNode: TTreeNode; Value: String): TTreeNode;
  function TreeNodeLocateOnLevel(TreeView: TTreeView; OnLevel: Integer; Value: String): TTreeNode;
  function TreeNodeGetChildFromIndex(TreeView: TTreeView; ParentNode: TTreeNode; ChildIndex: Integer): TTreeNode;
  function TreeNodeGetParentOnLevel(ChildNode: TTreeNode; ParentLevel: Integer): TTreeNode;
  procedure TreeNodeCopy(FromNode: TTreeNode; ToNode: TTreeNode; const CopyChildren: Boolean = true; const CopySubChildren: Boolean = true);

  // RichEdit:
  procedure RichEditSetStr(aRichEdit: TRichEdit; FormatedString: String);
  procedure RichEditStringReplace(aRichEdit: TRichEdit; OldPattern, NewPattern: string; Flags: TReplaceFlags);

  // Others :
  function GetTopMostControlAtPos(FromControl: TWinControl; aControlPoint: TPoint): TControl;
  procedure CenterControl(aControl: TControl);
  function GetLastParent(aControl: TControl): TWinControl;
  function GetControlBitmap(aControl: TWinControl): TBitmap;
  function GetRichEditBitmap(aRichEdit: TRichEdit): TBitmap;

implementation

function StringsLocate(aList: TStrings; Value: String; Options: TcyLocateOptions): Integer;
var
  i : Integer;
  StrItem: String;
  FindPartialKey, FindCaseInsensitive: Boolean;
begin
  Result := -1;
  FindPartialKey := lPartialKey in Options;
  FindCaseInsensitive := lCaseInsensitive in Options;

  if FindCaseInsensitive then
    Value := AnsiUpperCase(Value);

  for i  := 0 to aList.Count - 1 do
  begin
    if FindCaseInsensitive
    then StrItem := AnsiUpperCase(aList[i])
    else StrItem := aList[i];

    if FindPartialKey then
    begin
      if Pos(Value, StrItem) = 1 then // Must start as StrItem
      begin
        Result := i;
        Break;
      end;
    end
    else
      if StrItem = Value then
      begin
        Result := i;
        Break;
      end;
  end;
end;

function StringsLocate(aList: TStrings; Value: String; ValueKind: TStringsValueKind): Integer;
var
  i: Integer;
  fValue, f: Extended;
begin
  Result := -1;

  case ValueKind of
    skStringSensitive:
      Result := aList.IndexOf(Value);

    skStringInsensitive:
    begin
      Value := AnsiUppercase(Value);

      for i := 0 to aList.Count -1 do
        if Value = AnsiUppercase(aList[i]) then
        begin
          Result := i;
          Break;
        end;
    end;

    skExtended:
    begin
      if TryStrToFloat(Value, fValue) then
        for i := 0 to aList.Count -1 do
          if TryStrToFloat(aList[i], f) then
          if fValue = f then
            begin
              Result := i;
              Break;
            end;
    end;
  end;
end;

function StringsGetInsertPosition(aList: TStrings; Value: String; SortType: TStringsSortType): Integer;
var
  i: Integer;
  fValue, f: Extended;
begin
  Result := aList.Count;

  case SortType of
    stStringSensitive:
    begin
      for i := 0 to aList.Count -1 do
        if Value < aList[i] then
        begin
          Result := i;
          Break;
        end;
    end;

    stStringInsensitive:
    begin
      Value := AnsiUppercase(Value);

      for i := 0 to aList.Count -1 do
        if Value < AnsiUppercase(aList[i]) then
        begin
          Result := i;
          Break;
        end;
    end;

    stExtended:
    begin
      if TryStrToFloat(Value, fValue) then
        for i := 0 to aList.Count -1 do
          if TryStrToFloat(aList[i], f) then
          if fValue < f then
            begin
              Result := i;
              Break;
            end;
    end;
  end;
end;

function StringsAdd(aList: TStrings; Value: String; Unique: Boolean; SortType: TStringsSortType): Integer;
begin
  Result := -1;

  if Unique then
    case SortType of
      stNone, stStringSensitive: if aList.IndexOf(Value) <> -1 then Exit;
      stStringInsensitive: if StringsLocate(aList, Value, skStringInsensitive) <> -1 then Exit;
      stExtended: if StringsLocate(aList, Value, skExtended) <> -1 then Exit;
    end;

  if SortType <> stNone then
  begin
    Result := StringsGetInsertPosition(aList, Value, SortType);
    aList.Insert(Result, Value);
  end
  else
    Result := aList.Add(Value);
end;

procedure StringsReplace(aList: TStrings; OldStr: String; NewStr: String; ValueKind: TStringsValueKind);
var
  i: Integer;
  fValue, f: Extended;
begin
  case ValueKind of
    skStringSensitive:
    begin
      for i := 0 to aList.Count - 1 do
        if aList[i] = OldStr then
          aList[i] := NewStr;
    end;

    skStringInsensitive:
    begin
      for i := 0 to aList.Count - 1 do
        if AnsiUpperCase(aList[i]) = AnsiUpperCase(OldStr) then
          aList[i] := NewStr;
    end;

    skExtended:
    begin
      if TryStrToFloat(OldStr, fValue) then
        for i := 0 to aList.Count - 1 do
          if TryStrToFloat(aList[i], f) then
            if f = fValue then
              aList[i] := NewStr;
    end;
  end;
end;

procedure StringsSort(aList: TStrings; SortType: TStringsSortType);
var
  i, j, smallest: Integer;
  Str: String;
  f, smallVal: Extended;
begin
  case SortType of
    stStringSensitive:
      for i := 0 to aList.Count - 2 do
      begin
        smallest := i;

        for j := i + 1 to aList.Count - 1 do
          if aList[j] < aList[smallest] then
            smallest := j;

        if smallest <> i then
        begin
          Str := aList[i];
          aList[i] := aList[smallest];
          aList[smallest] := Str;
        end;
      end;

    stStringInsensitive:
      for i := 0 to aList.Count - 2 do
      begin
        smallest := i;

        for j := i + 1 to aList.Count - 1 do
          if AnsiUpperCase(aList[j]) < AnsiUpperCase(aList[smallest]) then
            smallest := j;

        if smallest <> i then
        begin
          Str := aList[i];
          aList[i] := aList[smallest];
          aList[smallest] := Str;
        end;
      end;

    stExtended:
      for i := 0 to aList.Count - 2 do
      begin
        smallest := i;

        if TryStrToFloat(aList[i], smallVal) then
        begin
          for j := i + 1 to aList.Count - 1 do
            if TryStrToFloat(aList[j], f)
            then
              if f < smallVal then
              begin
                smallest := j;
                smallVal := f;
              end;
        end
        else
          smallest := aList.Count - 1;

        if smallest <> i then
        begin
          Str := aList[i];
          aList[i] := aList[smallest];
          aList[smallest] := Str;
        end;
      end;
  end;
end;

function TreeNodeLocate(ParentNode: TTreeNode; Value: String): TTreeNode;
begin
  Result := ParentNode.GetFirstChild;

  while Result <> nil do
  begin
    if Result.Text = Value
    then Break
    else Result := ParentNode.GetNextChild(Result);
  end;
end;

function TreeNodeLocateOnLevel(TreeView: TTreeView; OnLevel: Integer; Value: String): TTreeNode;
var i : Integer;
begin
  Result := Nil;

  for i := 0 to TreeView.Items.Count - 1 do
    if TreeView.Items[i].Level = OnLevel
    then
      if TreeView.Items[i].Text = Value then
      begin
        Result := TreeView.Items[i];
        Break;
      end;
end;

function TreeNodeGetChildFromIndex(TreeView: TTreeView; ParentNode: TTreeNode; ChildIndex: Integer): TTreeNode;
var _Child: TTreeNode;
begin
  Result := Nil;

  if ParentNode = nil
  then _Child := TreeView.Items[0]
  else _Child := ParentNode.getFirstChild;

  while (_Child <> nil) and (Result = nil) do
    if _Child.Index = ChildIndex
    then Result := _Child
    else _Child := _Child.GetNextChild(_Child);
end;

function TreeNodeGetParentOnLevel(ChildNode: TTreeNode; ParentLevel: Integer): TTreeNode;
begin
  Result := Nil;

  if ChildNode <> nil then
  begin
    while (ChildNode.Level <> ParentLevel) and (ChildNode.Level > 0) do
      ChildNode := ChildNode.Parent;

    if ChildNode.Level = ParentLevel then
      Result := ChildNode;
  end;
end;

procedure TreeNodeCopy(FromNode: TTreeNode; ToNode: TTreeNode; const CopyChildren: Boolean = true; const CopySubChildren: Boolean = true);
var Child: TTreeNode;
begin
  ToNode.ImageIndex    := FromNode.ImageIndex;
  ToNode.SelectedIndex := FromNode.SelectedIndex;
  ToNode.StateIndex    := FromNode.StateIndex;
  ToNode.Text          := FromNode.Text;

  if CopyChildren and FromNode.HasChildren then
  begin
    Child := FromNode.GetFirstChild;

    while Child <> Nil do
    begin
      if CopySubChildren then
        TreeNodeCopy(Child, ToNode.Owner.AddChild(ToNode, ''), true, true);

      Child := FromNode.GetNextChild(Child);
    end;
  end;
end;

procedure RichEditSetStr(aRichEdit: TRichEdit; FormatedString: String);
var StringStream: TStringStream;
begin
  {$IFDEF DELPHI2009_OR_ABOVE}
  StringStream := TStringStream.Create(FormatedString, TEncoding.UTF8);  // Dot not use TEncoding.Unicode !
  aRichedit.Lines.LoadFromStream(StringStream, TEncoding.UTF8);
  {$ELSE}
  StringStream := TStringStream.Create(FormatedString);
  aRichedit.Lines.LoadFromStream(StringStream);
  {$ENDIF}
  StringStream.Free;
end;

procedure RichEditStringReplace(aRichEdit: TRichEdit; OldPattern, NewPattern: string; Flags: TReplaceFlags);
var
  X, TextLength: integer;
  Options: TSearchTypes;
begin
  Options := [];

  if not (rfIgnoreCase in Flags) then
  Include(Options, stMatchCase);

  with aRichEdit do
  begin
    X := 0;
    TextLength := length(Text);
    X := FindText(OldPattern, X, TextLength, Options);
    while X <> -1 do
    begin
      SelStart := X;
      SelLength := length(OldPattern);
      SelText := NewPattern;

      if rfReplaceAll in Flags
      then X := FindText(OldPattern, X + length(NewPattern), TextLength, Options)
      else X := -1;
     end;
   end;
end;

function GetTopMostControlAtPos(FromControl: TWinControl; aControlPoint: TPoint): TControl;
var FoundControl: TControl;
begin
  RESULT := nil;

  while FromControl <> nil do
  begin
    FoundControl := FromControl.ControlAtPos(aControlPoint, True, true);

    if FoundControl <> nil
    then begin
      RESULT := FoundControl;

      if FoundControl is TWinControl
      then begin
        aControlPoint := FromControl.ClientToScreen(aControlPoint);
        FromControl := TWinControl(RESULT);
        aControlPoint := FromControl.ScreenToClient(aControlPoint);
      end
      else
        FromControl := Nil;
    end
    else
      FromControl := nil;
  end;
end;

function GetLastParent(aControl: TControl): TWinControl;
begin
  Result := aControl.Parent;

  if Result <> Nil then
    while Result.Parent <> nil do
      Result := RESULT.Parent;
end;

procedure CenterControl(aControl: TControl);
begin
  if aControl.Parent = nil then
  begin
    aControl.Left := (Screen.Width - aControl.Width) Div 2;
    aControl.Top  := (Screen.Height - aControl.Height) Div 2;
  end
  else begin
    aControl.Left := (aControl.Parent.Width - aControl.Width) Div 2;
    aControl.Top  := (aControl.Parent.Height - aControl.Height) Div 2;
  end;
end;

function GetControlBitmap(aControl: TWinControl): TBitmap;
var
  Ofs, c, l, t: Integer;
  aChildBmp: TBitmap;
begin
  if (aControl is TRichEdit) or ((AnsiUpperCase(aControl.ClassName) = 'TDBRICHEDIT')) then
  begin
    Result := GetRichEditBitmap(TRichEdit(aControl));
    Exit;
  end;

  Result := TBitmap.Create;

  try
    with aControl do
    begin
      Result.Width := ClientWidth;
      Result.Height := ClientHeight;
      Result.Canvas.Brush := Brush;
      Result.Canvas.FillRect(ClientRect);
      Result.Canvas.Lock;
      try
        if GetWindowLong(Handle, GWL_STYLE) and WS_BORDER <> 0 then
          Ofs := -1  // Don't draw form border
        else
          Ofs := 0;  // There is no border
        PaintTo(Result.Canvas.Handle, Ofs, Ofs);
      finally
        Result.Canvas.Unlock;
      end;

      // Draw not handled TRichEdit component:
      for c := 0 to ControlCount-1 do
        if (Controls[c] is TRichEdit) or ((AnsiUpperCase(Controls[c].ClassName) = 'TDBRICHEDIT')) then
        begin
          aChildBmp := GetRichEditBitmap(TRichEdit(Controls[c]));
          l := Controls[c].Left;
          t := Controls[c].Top;
          with TRichEdit(Controls[c]) do
          begin
            inc(l, (Width - ClientWidth) div 2);
            inc(t, (Height - ClientHeight) div 2);
          end;
          Result.Canvas.Draw(l, t, aChildBmp);
          aChildBmp.Free;
        end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function GetRichEditBitmap(aRichEdit: TRichEdit): TBitmap;
var
  fPixelsperInch: Integer;
  FormatRange: TFormatRange;

        function PixelsToTwips(PixelValue: Integer): Integer; // (Twips = 1/1440 inch)
        begin
          Result := PixelValue * 1440 div fPixelsperInch;
        end;

begin
  Result := TBitmap.Create;
  fPixelsperInch := GetDeviceCaps(Result.Canvas.Handle, LOGPIXELSX);

  try
    with aRichEdit do
    begin
      Result.Width := ClientWidth;
      Result.Height := ClientHeight;
      Result.Canvas.Brush := Brush;
      Result.Canvas.FillRect(ClientRect);
      Result.Canvas.Lock;
      try
        FormatRange.hdc := Result.Canvas.Handle;
        FormatRange.hdcTarget := Result.Canvas.Handle;
        // rc is the destination rect on the DBGrid:
        FormatRange.rc := classes.Rect(0, 0, PixelsToTwips(Result.Width), PixelsToTwips(Result.Height));
        FormatRange.rcPage := FormatRange.rc;
        FormatRange.chrg.cpMin := 0;     // First car
        FormatRange.chrg.cpMax := aRichEdit.GetTextLen;
        aRichEdit.Perform(EM_FORMATRANGE, 1, Longint(@FormatRange));   // Draw text
        aRichEdit.Perform(EM_FORMATRANGE, 0, 0);  // Free cached data
      finally
        Result.Canvas.Unlock;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
