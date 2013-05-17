{   Component(s):
    TcyCustomIndependantBookmarkList
    Description:
    Almost like Delphi TBookmarkList with some new abilities(insert/delete bookmark).
    It is linked directly to a DataSource instead of a TCustomDBGrid.
    OnChange property event can be used for notify changes 

    ************************ IMPORTANT ************************
    *  This component source code was copied from original    *
    *  Delphi TBookmarkList component (unit 'dbgrids').       *
    *  Only few modifications was made to correspond to what  *
    *  i wanted to do.                                        *
    ***********************************************************

    TcyBookmarkList
    Description:
    Herited of TcyCustomIndependantBookmarkList with some public declarations

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

unit cyBookmarkListD2007;

interface

uses Classes, Db, DbConsts;

type
  TcyCustomIndependantBookmarkList = class
  private
    FList: TStringList;
    FCache: TBookmarkStr;
    FCacheIndex: Integer;
    FCacheFind: Boolean;
    FLinkActive: Boolean;
    FDataSource: TDataSource;
    FOnChange: TNotifyEvent;
    function GetCount: Integer;
    function GetCurrentRecordBookmarked: Boolean;
    function GetItem(Index: Integer): TBookmarkStr;
    procedure StringsChanged(Sender: TObject);
    procedure SetDataSource(const Value: TDataSource);
  protected
    function Compare(const Item1, Item2: TBookmarkStr): Integer;
    property DataSource: TDataSource read FDataSource write SetDataSource;
  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;
    procedure RaiseLinkError;
    procedure Clear; virtual;
    function CurrentRecord: TBookmarkStr;
    procedure LinkActive(Value: Boolean); virtual;
    procedure DeleteBookmarkedRecords; virtual;
    procedure SetCurrentRecordBookmarked(Value: Boolean);
    function InsertBookmark(aBookmark: TBookmarkStr): Boolean; virtual;
    function DeleteBookmark(aBookmark: TBookmarkStr): Boolean; virtual;
    function DeleteFromIndex(Index: Word): Boolean; virtual;
    function Find(const Item: TBookmarkStr; var Index: Integer): Boolean;
    function IndexOf(const Item: TBookmarkStr): Integer;
    function Refresh: Boolean; virtual; // drop orphaned bookmarks; True = orphans found
    procedure CurrentRecordSwitch;
    property Count: Integer read GetCount;
    property CurrentRecordBookmarked: Boolean read GetCurrentRecordBookmarked write SetCurrentRecordBookmarked;
    property Items[Index: Integer]: TBookmarkStr read GetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TcyBookmarkList = class(TcyCustomIndependantBookmarkList)
  public
    property DataSource;
  end;

implementation

{ TcyCustomIndependantBookmarkList }
constructor TcyCustomIndependantBookmarkList.Create(Owner: TComponent);
begin
  inherited Create;
  FList := TStringList.Create;
  FList.OnChange := StringsChanged;
end;

destructor TcyCustomIndependantBookmarkList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TcyCustomIndependantBookmarkList.RaiseLinkError;
begin
  raise EComponentError.Create('TcyCustomIndependantBookmarkList not linked!')
end;

procedure TcyCustomIndependantBookmarkList.Clear;
begin
  if FList.Count = 0 then Exit;
  FList.Clear;

  if Assigned(FOnChange)
  then FOnChange(Self);
end;

procedure TcyCustomIndependantBookmarkList.SetDataSource(const Value: TDataSource);
begin
  if FLinkActive
  then LinkActive(false);

  FDataSource := Value;
end;

function TcyCustomIndependantBookmarkList.Compare(const Item1, Item2: TBookmarkStr): Integer;
begin
  with Datasource.Dataset do
    Result := CompareBookmarks(TBookmark(Item1), TBookmark(Item2));
end;

function TcyCustomIndependantBookmarkList.CurrentRecord: TBookmarkStr;
begin
  if not FLinkActive then RaiseLinkError;
  RESULT := DataSource.Dataset.Bookmark;
end;

function TcyCustomIndependantBookmarkList.GetCurrentRecordBookmarked: Boolean;
var
  Index: Integer;
begin
  if not FLinkActive then RaiseLinkError;
  Result := Find(CurrentRecord, Index);
end;

function TcyCustomIndependantBookmarkList.Find(const Item: TBookmarkStr; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  if (Item = FCache) and (FCacheIndex >= 0) then
  begin
    Index := FCacheIndex;
    Result := FCacheFind;
    Exit;
  end;
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(FList[I], Item);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
  FCache := Item;
  FCacheIndex := Index;
  FCacheFind := Result;
end;

function TcyCustomIndependantBookmarkList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcyCustomIndependantBookmarkList.GetItem(Index: Integer): TBookmarkStr;
begin
  Result := FList[Index];
end;

function TcyCustomIndependantBookmarkList.IndexOf(const Item: TBookmarkStr): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

procedure TcyCustomIndependantBookmarkList.LinkActive(Value: Boolean);
begin
  Clear;

  if Value
  then begin
    if FDataSource <> nil
    then
      if FDataSource.DataSet <> nil
      then
        if FDataSource.DataSet.Active
        then FLinkActive := true;

    if not FLinkActive
    then raise EComponentError.Create('TcyCustomIndependantBookmarkList without DataSource!');
  end
  else
    FLinkActive := false;
end;

procedure TcyCustomIndependantBookmarkList.DeleteBookmarkedRecords;
var
  I: Integer;
begin
  if FList.Count = 0 then EXIT;
  
  with Datasource.Dataset do
  begin
    DisableControls;
    try
      for I := FList.Count-1 downto 0 do
      begin
        Bookmark := FList[I];
        Delete;
        FList.Delete(I);
      end;
    finally
      EnableControls;
    end;
  end;

  if Assigned(FOnChange)
  then FOnChange(Self);
end;

function TcyCustomIndependantBookmarkList.Refresh: Boolean;
var
  I: Integer;
begin
  Result := False;
  with Datasource.Dataset do
  try
    CheckBrowseMode;
    for I := FList.Count - 1 downto 0 do
      if not BookmarkValid(TBookmark(FList[I])) then
      begin
        Result := True;
        FList.Delete(I);
      end;
  finally
    UpdateCursorPos;
  end;

  if RESULT
  then
    if Assigned(FOnChange)
    then FOnChange(Self);
end;

function TcyCustomIndependantBookmarkList.InsertBookmark(aBookmark: TBookmarkStr): Boolean;
var
  Index: Integer;
begin
  RESULT := false;
  if not FLinkActive then RaiseLinkError;

  if (Length(aBookmark) <> 0) and (Find(aBookmark, Index) = false)
  then begin
    RESULT := true;
    FList.Insert(Index, aBookmark);

    if Assigned(FOnChange)
    then FOnChange(Self);
  end;
end;

function TcyCustomIndependantBookmarkList.DeleteBookmark(aBookmark: TBookmarkStr): Boolean;
var
  Index: Integer;
begin
  RESULT := false;
  if not FLinkActive then RaiseLinkError;

  if (Length(aBookmark) <> 0) and (Find(aBookmark, Index) = true)
  then begin
    RESULT := true;
    FList.Delete(Index);

    if Assigned(FOnChange)
    then FOnChange(Self);
  end;
end;

function TcyCustomIndependantBookmarkList.DeleteFromIndex(Index: Word): Boolean;
begin
  RESULT := false;

  if Index < FList.Count
  then begin
    RESULT := true;
    FList.Delete(Index);

    if Assigned(FOnChange)
    then FOnChange(Self);
  end;
end;

procedure TcyCustomIndependantBookmarkList.CurrentRecordSwitch;
begin
  CurrentRecordBookmarked := not CurrentRecordBookmarked;
end;

procedure TcyCustomIndependantBookmarkList.SetCurrentRecordBookmarked(Value: Boolean);
var
  Index: Integer;
  Current: TBookmarkStr;
begin
  if not FLinkActive then RaiseLinkError;

  Current := CurrentRecord;
  if (Length(Current) = 0) or (Find(Current, Index) = Value) then Exit;

  if Value
  then FList.Insert(Index, Current)
  else FList.Delete(Index);

  if Assigned(FOnChange)
  then FOnChange(Self);
end;

procedure TcyCustomIndependantBookmarkList.StringsChanged(Sender: TObject);
begin
  FCache := '';
  FCacheIndex := -1;
end;

end.
