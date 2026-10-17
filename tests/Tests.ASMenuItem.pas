unit Tests.ASMenuItem;

{$mode objfpc}{$H+}

{ Behaviour tests for TASMenuItem (Kernel.ASMenuItem).

  The menu item subclass only stores node references and manages its own
  separator lines, so everything runs headless on real menu items. }

interface

uses
  Classes, SysUtils, Menus, fpcunit, testregistry,
  VirtualTrees,
  NodeDataTypes.Category,
  Kernel.ASMenuItem;

type
  TTestASMenuItem = class(TTestCase)
  published
    procedure TestDataRoundTrip;
    procedure TestPNodeRoundTrip;
    procedure TestPathRoundTrip;
    procedure TestNewBottomLineOnEmpty;
    procedure TestNewBottomLineAppendsLine;
    procedure TestNewBottomLineSkipsExistingLine;
    procedure TestInsertNewLineBeforeRevealsHidden;
    procedure TestInsertNewLineAfterInserts;
  end;

implementation

procedure TTestASMenuItem.TestDataRoundTrip;
var
  Item: TASMenuItem;
  Data: TvCategoryNodeData;
begin
  Item := TASMenuItem.Create(nil);
  Data := TvCategoryNodeData.Create;
  try
    AssertTrue('no data initially', Item.Data = nil);
    Item.Data := Data;
    AssertTrue('data stored', Item.Data = Data);
  finally
    Data.Free;
    Item.Free;
  end;
end;

procedure TTestASMenuItem.TestPNodeRoundTrip;
var
  Item: TASMenuItem;
begin
  Item := TASMenuItem.Create(nil);
  try
    AssertTrue('no node initially', Item.pNode = nil);
  finally
    Item.Free;
  end;
end;

procedure TTestASMenuItem.TestPathRoundTrip;
var
  Item: TASMenuItem;
begin
  Item := TASMenuItem.Create(nil);
  try
    Item.Path := '/tmp/x';
    AssertEquals('path stored', '/tmp/x', UTF8Encode(Item.Path));
  finally
    Item.Free;
  end;
end;

procedure TTestASMenuItem.TestNewBottomLineOnEmpty;
var
  Item: TASMenuItem;
begin
  Item := TASMenuItem.Create(nil);
  try
    Item.NewBottomLine;
    AssertEquals('line added', 1, Item.Count);
    AssertTrue('it is a separator', Item.Items[0].IsLine);
  finally
    Item.Free;
  end;
end;

procedure TTestASMenuItem.TestNewBottomLineAppendsLine;
var
  Item: TASMenuItem;
  Entry: TMenuItem;
begin
  Item := TASMenuItem.Create(nil);
  try
    Entry := TMenuItem.Create(Item);
    Entry.Caption := 'entry';
    Item.Add(Entry);
    Item.NewBottomLine;
    AssertEquals('line appended', 2, Item.Count);
    AssertTrue('last is a separator', Item.Items[1].IsLine);
  finally
    Item.Free;
  end;
end;

procedure TTestASMenuItem.TestNewBottomLineSkipsExistingLine;
var
  Item: TASMenuItem;
begin
  Item := TASMenuItem.Create(nil);
  try
    Item.NewBottomLine;
    Item.NewBottomLine;
    AssertEquals('no duplicate line', 1, Item.Count);
  finally
    Item.Free;
  end;
end;

procedure TTestASMenuItem.TestInsertNewLineBeforeRevealsHidden;
var
  Item: TASMenuItem;
  Entry, Line: TMenuItem;
begin
  Item := TASMenuItem.Create(nil);
  try
    Line := NewLine;
    Line.Visible := False;
    Item.Add(Line);
    Entry := TMenuItem.Create(Item);
    Entry.Caption := 'entry';
    Item.Add(Entry);
    // A hidden line just before is revealed instead of duplicated
    Item.InsertNewLine(True, Entry);
    AssertEquals('no new item', 2, Item.Count);
    AssertTrue('line revealed', Item.Items[0].Visible);
  finally
    Item.Free;
  end;
end;

procedure TTestASMenuItem.TestInsertNewLineAfterInserts;
var
  Item: TASMenuItem;
  Entry: TMenuItem;
begin
  Item := TASMenuItem.Create(nil);
  try
    Entry := TMenuItem.Create(Item);
    Entry.Caption := 'entry';
    Item.Add(Entry);
    Item.InsertNewLine(False, Entry);
    AssertEquals('line inserted', 2, Item.Count);
    AssertTrue('line after the entry', Item.Items[1].IsLine);
  finally
    Item.Free;
  end;
end;

initialization
  RegisterTest(TTestASMenuItem);

end.
