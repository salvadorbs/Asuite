{
Copyright (C) 2006-2013 Matteo Salvi

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

unit DataModules.Images;

{$I ASuite.inc}

interface

uses
  SysUtils, Classes, Controls, Windows, Graphics, Dialogs, Kernel.Enumerations,
  NodeDataTypes.Custom, NodeDataTypes.Base, ShellApi, CommCtrl, Vcl.ImgList, ulCommonUtils, VirtualTrees,
  DKLang;

type
  TRGBArray = array[Word] of TRGBTriple;
  pRGBArray = ^TRGBArray;

  TResType = (rtBMP, rtPNG, rtICO);

  TdmImages = class(TDataModule)
    IcoImages: TImageList;
    LargeIcoImages: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmImages: TdmImages;

implementation

uses
  Kernel.Consts, AppConfig.Main, Utility.Treeview;

{$R *.dfm}

end.

