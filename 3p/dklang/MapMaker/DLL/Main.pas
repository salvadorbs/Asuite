///*********************************************************************************************************************
///  $Id: Main.pas,v 1.2 2006-06-17 04:19:28 dale Exp $
///---------------------------------------------------------------------------------------------------------------------
///  DKLang Localization Package
///  Copyright 2002-2006 DK Software, http://www.dk-soft.org
///*********************************************************************************************************************
///
/// The contents of this package are subject to the Mozilla Public License
/// Version 1.1 (the "License"); you may not use this file except in compliance
/// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
///
/// Alternatively, you may redistribute this library, use and/or modify it under the
/// terms of the GNU Lesser General Public License as published by the Free Software
/// Foundation; either version 2.1 of the License, or (at your option) any later
/// version. You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/
///
/// Software distributed under the License is distributed on an "AS IS" basis,
/// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
/// specific language governing rights and limitations under the License.
///
/// The initial developer of the original code is Dmitry Kann, http://www.dk-soft.org/
///
///**********************************************************************************************************************
unit Main;

interface

const
  SResourceMapFileExtension = '.drc';


   // Exported function. Processes the file named the same as pcExeName but with the .drc extension (those files are
   //   created automatically by the IDE during compile; you should either have turned on Detailed map Linker option, or
   //   supply -GD or --drc option to the command-line compiler). The created map is then written into the executable's
   //   resources.
   //   Returns True on successful completion, False otherwise.
  function MakeResMap(pcExeName: PAnsiChar): LongBool; stdcall;

implementation
uses SysUtils, Classes;

  function MakeResMap(pcExeName: PAnsiChar): LongBool;
  var
    SLInputText, SLResourceMap: TStringList;
    i: Integer;
    sLine, sName, sIdent: String;

     // Extract and returns the first (trimmed) word from space-delimited string s
    function ExtractWord(var s: String): String;
    var i: Integer;
    begin
      i := Pos(' ', s);
      if i=0 then i := Length(s)+1;
      Result := Trim(Copy(s, 1, i-1));
      Delete(s, 1, i);
    end;

  begin
    Result := True;
    try
      SLInputText := TStringList.Create;
      try
        SLResourceMap := TStringList.Create;
        try
           // Read the .drc file
          SLInputText.LoadFromFile(ChangeFileExt(pcExeName, SResourceMapFileExtension));
           // Parse the file line-by-line
          for i := 0 to SLInputText.Count-1 do begin
            sLine := SLInputText[i];
             // If it's the #define map entry
            if ExtractWord(sLine)='#define' then begin
              sName  := ExtractWord(sLine);
              sIdent := ExtractWord(sLine);
               // If there are valid values, translate it into the form 'Ident=Name'
              if IsValidIdent(sName) and (StrToIntDef(sIdent, 0)>0) then
                SLResourceMap.Add(Format('%s=%s', [sIdent, sName]));
            end;
          end;
          {!!!}SLResourceMap.SaveToFile(ChangeFileExt(pcExeName, '.resmap'));
        finally
          SLResourceMap.Free;
        end;         
      finally
        SLInputText.Free;
      end;
    except
      Result := False;
    end;
  end;

end.
 