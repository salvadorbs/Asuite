Unit ExtCtrls;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL ExtCtrls.pas
   Just put the LVCL directory in your Project/Options/Path/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TImage
   - compatible with the standard .DFM files.
   - only use existing properties in your DFM, otherwize you'll get error on startup

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (C) 2008 Arnaud Bouchez - http://bouchez.info
  Emulates the original Delphi/Kylix Cross-Platform Runtime Library
  (c)2000,2001 Borland Software Corporation
  Portions created by Paul Toth are Copyright (C) 2001 Paul Toth. http://tothpaul.free.fr
  All Rights Reserved.

}

interface

uses
  SysUtils, Classes, Controls, Graphics;

type
  TImage = class(TGraphicControl)
  private
    fPicture: TPicture;
    function GetPicture: TPicture;
  protected
    function SubProperty(const Name: string): TPersistent; override;
  public
    destructor Destroy; override;
    procedure Paint; override;
    property Picture: TPicture read GetPicture;
  end;


implementation

destructor TImage.Destroy;
begin
  fPicture.Free;
end;

function TImage.GetPicture:TPicture;
begin
  if fPicture=nil then
    fPicture := TPicture.Create;
  result := fPicture;
end;

function TImage.SubProperty(const Name: string):TPersistent;
const
  TPictureSubProperties: array[0..0] of PChar =(
   'Picture'
  );
begin
  case StringIndex(Name,TPictureSubProperties) of
   0 : result := Picture;
   else result := inherited SubProperty(Name);
  end;
end;

procedure TImage.Paint;
begin
  if (fPicture=nil) or not fVisible then
    inherited else
    fPicture.DrawRect(ClientRect,Canvas); // not VCL standard, but works for BITMAP
end;

initialization
  RegisterClasses([TImage]);
end.
