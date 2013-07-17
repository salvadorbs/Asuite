{   Component(s):
    tcyBookmarks

    Description:
    Allow to bookmark records from a DataSource.

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

unit cyBookmarks;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses classes, Messages, Controls, DB, {$IFDEF DELPHI2009_OR_ABOVE} cyBookmarkList; {$ELSE} cyBookmarkListD2007; {$ENDIF}

type
  TcyBookmarks = class;

  TcyBookmarksDataLink = class(TDataLink)
  private
    FBookmarks: TcyBookmarks;
  protected
    procedure ActiveChanged; override;
  public
    constructor Create(aBookmarks: TcyBookmarks);
    destructor Destroy; override;
  end;

  TcyBookmarks = class(TComponent)
  private
    FBookmarkList: TcyBookmarkList;
    FDataLink: TcyBookmarksDataLink;
    procedure SetDataSource(const Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    function GetDataSource: TDataSource;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure LinkActive(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BookmarkList: TcyBookmarkList read FBookmarkList;
    function  IsCurrentRecordBookmarked: Boolean;
    function  AddCurrentRecord: Boolean;
    function  RemoveCurrentRecord: Boolean;
    procedure SwitchCurrentRecord;
    procedure RemoveAll;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation

{TcyBookmarksDataLink}
constructor TcyBookmarksDataLink.Create(aBookmarks: TcyBookmarks);
begin
  inherited Create;
  FBookmarks := aBookmarks;
  VisualControl := false;
end;

destructor TcyBookmarksDataLink.Destroy;
begin
  inherited Destroy;
end;

procedure TcyBookmarksDataLink.ActiveChanged;
begin
  Inherited;
  FBookmarks.LinkActive(Active);
end;

{TcyBookmarks}
constructor TcyBookmarks.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TcyBookmarksDataLink.Create(Self);
  FBookmarkList := TcyBookmarkList.Create(self);
end;

destructor TcyBookmarks.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FBookmarkList.Free;
  FBookmarkList := nil;
  inherited Destroy;
end;

procedure TcyBookmarks.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TcyBookmarks.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TcyBookmarks.LinkActive(Value: Boolean);
begin
  FBookmarkList.DataSource := DataSource;
  FBookmarkList.LinkActive(Value);
end;

function TcyBookmarks.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TcyBookmarks.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TcyBookmarks.IsCurrentRecordBookmarked: boolean;
begin
  RESULT := FBookmarkList.CurrentRecordBookmarked;
end;

function TcyBookmarks.AddCurrentRecord: Boolean;
begin
  RESULT := FBookmarkList.InsertBookmark(FBookmarkList.CurrentRecord);
end;

function TcyBookmarks.RemoveCurrentRecord: Boolean;
begin
  RESULT := FBookmarkList.DeleteBookmark(FBookmarkList.CurrentRecord);
end;

procedure TcyBookmarks.SwitchCurrentRecord;
begin
  FBookmarkList.CurrentRecordSwitch;
end;

procedure TcyBookmarks.RemoveAll;
begin
  FBookmarkList.Clear;
end;

end.
