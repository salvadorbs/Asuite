{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit OrderSoftware;

{$MODE Delphi}

interface

uses
  Forms, StdCtrls, Buttons, ulCommonClasses, ComCtrls, ulEnumerations;

type
  TfrmOrderSoftware = class(TForm)
    lstStartUp: TListBox;
    btnOk: TButton;
    btnCancel: TButton;
    pgcSoftwareOrder: TPageControl;
    tsStartUp: TTabSheet;
    tsShutdown: TTabSheet;
    lstShutdown: TListBox;
    btnUp: TBitBtn;
    btnDown: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bbtnUpClick(Sender: TObject);
    procedure bbtnDownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAutorunType: TAutorunType;
    procedure PopulateLstAutoExe(ListBox: TListBox;AutorunItemList: TAutorunItemList);
    procedure SaveInAutorunItemList(ListBox: TListBox;AutorunItemList: TAutorunItemList);
    procedure MoveItemUp(ListBox: TListBox);
    procedure MoveItemDown(ListBox: TListBox);
    { Private declarations }
  public
    property AutorunType: TAutorunType read FAutorunType write FAutorunType;
    { Public declarations }
  end;

var
  frmOrderSoftware : TfrmOrderSoftware;

implementation

uses
  ulNodeDataTypes, ulTreeView;

{$R *.lfm}

procedure TfrmOrderSoftware.bbtnDownClick(Sender: TObject);
begin
  case pgcSoftwareOrder.ActivePageIndex of
    0:
      MoveItemDown(lstStartUp);
    1:
      MoveItemDown(lstShutdown);
  end;
end;

procedure TfrmOrderSoftware.bbtnUpClick(Sender: TObject);
begin
  case pgcSoftwareOrder.ActivePageIndex of
    0:
      MoveItemUp(lstStartUp);
    1:
      MoveItemUp(lstShutdown);
  end;
end;

procedure TfrmOrderSoftware.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOrderSoftware.btnOkClick(Sender: TObject);
begin
  //Save Startup and Shutdown lists
  SaveInAutorunItemList(lstStartUp,ASuiteStartUpApp);
  SaveInAutorunItemList(lstShutdown,ASuiteShutdownApp);
  Close;
end;

procedure TfrmOrderSoftware.FormCreate(Sender: TObject);
begin

end;

procedure TfrmOrderSoftware.PopulateLstAutoExe(ListBox: TListBox;AutorunItemList: TAutorunItemList);
var
  I: Integer;
  NodeData: TvFileNodeData;
begin
  ListBox.Items.BeginUpdate;
  for I := 0 to AutorunItemList.Count - 1 do
  begin
    NodeData := AutorunItemList[I];
    if Assigned(NodeData) then
      ListBox.Items.AddObject(NodeData.Name, AutorunItemList[I]);
  end;
  ListBox.Items.EndUpdate;
end;

procedure TfrmOrderSoftware.SaveInAutorunItemList(ListBox: TListBox;AutorunItemList: TAutorunItemList);
var
  I: Integer;
  NodeData: TvFileNodeData;
begin
  AutorunItemList.Clear;
  for I := 0 to ListBox.Count - 1 do
  begin
    NodeData := TvFileNodeData(ListBox.Items.Objects[I]);
    NodeData.AutorunPos := AutorunItemList.Add(NodeData);
    NodeData.Changed := True;
  end;
end;

procedure TfrmOrderSoftware.MoveItemUp(ListBox: TListBox);
var
  CurrIndex: Integer;
begin
  with ListBox do
  begin
    CurrIndex := ItemIndex;
    if (CurrIndex <> (-1 and 0)) and (Count <> 0) then
    begin
      Items.Move(CurrIndex, CurrIndex - 1);
      Selected[CurrIndex - 1] := True;
    end;
  end;
end;

procedure TfrmOrderSoftware.MoveItemDown(ListBox: TListBox);
var
  CurrIndex: Integer;
begin
  with ListBox do
  begin
    CurrIndex := ItemIndex;
    if (CurrIndex <> (-1 and (Count - 1))) and (Count <> 0) then
    begin
      Items.Move(CurrIndex, CurrIndex + 1);
      Selected[CurrIndex + 1] := True;
    end;
  end;
end;

procedure TfrmOrderSoftware.FormShow(Sender: TObject);
begin
  //According as AutorunType, display StartUp or Shutdown page
  case FAutorunType of
    atAlwaysOnStart, atSingleInstance:
      pgcSoftwareOrder.ActivePageIndex := 0;
    atAlwaysOnClose:
      pgcSoftwareOrder.ActivePageIndex := 1;
  end;
  //Populate lstStartUp and lstShutdown
  PopulateLstAutoExe(lstStartUp,ASuiteStartUpApp);
  PopulateLstAutoExe(lstShutdown,ASuiteShutdownApp);
end;

end.
