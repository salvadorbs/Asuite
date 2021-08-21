{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Kernel.Types;

{$MODE DelphiUnicode}

interface

uses
  Generics.Collections, NodeDataTypes.Base, VirtualTrees, Generics.Defaults,
  Laz2_DOM, Types, Menus, Kernel.Enumerations, Frame.BaseEntity;

type
  TImportListToTree = function(Tree: TVirtualStringTree; Node: TDOMNode;
                               Parent: PVirtualNode): PVirtualNode of object;

  TNodeDataItems  = TList<TvBaseNodeData>;
  TItemsComparer  = TComparer<TvBaseNodeData>;

  TArrayRect  = Array of TRect;
  TArrayPoint = Array of TPoint;

  rBaseData = record
    Data     : TvBaseNodeData;
    MenuItem : TMenuItem; //Classic Menu
    MenuNode : PVirtualNode; //Graphic Menu
  end;
  PBaseData = ^rBaseData;

  rTreeDataX = record
    pNodeList : PVirtualNode;
  end;
  PTreeDataX = ^rTreeDataX; //X = Search or TrayMenu

  //Record for Options and Property form
  rFramesNodeData = record
    Title : string;
    Frame : TfrmBaseEntityPage;
    ImageIndex: Integer;
  end;
  PFramesNodeData = ^rFramesNodeData;

  //Record for ScanFolder's vst
  rScanFolderData = record
    Text: string;
    ImageIndex: Integer;
  end;
  PScanFolderData = ^rScanFolderData;

  TLauncherSearch = record
    Tree       : TBaseVirtualTree;
    Keyword    : string;
    SearchType : TSearchType;
  end;

  rListStats = record
    SwCount  : Integer;
    CatCount : Integer;
  end;
  PListStats = ^rListStats;

  TUserData = record
    UserName : string;
    Password : string;
  end;
  PUserData = ^TUserData;

  TUrlFile = record
    TargetFile : String;
    WorkingDir : String;
    PathIcon   : String;
  end;

implementation

end.
