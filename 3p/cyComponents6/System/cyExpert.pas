{    * ***** BEGIN LICENSE BLOCK *****
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

unit cyExpert;

interface

uses SysUtils;

// procedure IDESource_AddUnitToUses(aUnitName: AnsiString);

implementation

(* procedure IDESource_AddUnitToUses(aUnitName: AnsiString);

      function GetSourceEditor: IOTASourceEditor;
      var
        ModuleServices: IOTAModuleServices;
        Module: IOTAModule;
        Editor: IOTAEditor;
        i: Integer;
      begin
        RESULT := Nil;
        ModuleServices := BorlandIDEServices as IOTAModuleServices;
        for i := 0 to ModuleServices.ModuleCount-1 do
        begin
          Module := ModuleServices.Modules[i];
          if Assigned(Module)
          then begin
            Editor := Module.ModuleFileEditors[i];
            if Assigned(Editor)
            then
              if Editor.QueryInterface(IOTASourceEditor, RESULT) = S_OK
              then Break;
          end;
        end;
      end;

const
  BufferSize = 254;   // Number of chars for Buffer ...
var
  SourceEditor: IOTASourceEditor;
  Reader: IOTAEditReader;
  Writer: IOTAEditWriter;
  Buffer: AnsiString;
  Position, ReadedChars, UsesPos: Integer;
  UsesStr: AnsiString;
  SourceCode: String;
begin
  SourceEditor := GetSourceEditor;

  if SourceEditor = Nil
  then EXIT;

  SourceCode := '';
  Reader := SourceEditor.CreateReader;

  // Retrieve code source :
  if Assigned(Reader)
  then begin
    Position := 0;
    SetLength(Buffer, BufferSize);

    repeat
      ReadedChars := Reader.GetText(Position, PAnsiChar(Buffer), BufferSize);
      Inc(Position, ReadedChars);
      SourceCode := SourceCode + Copy(Buffer, 1, ReadedChars);
    until ReadedChars < BufferSize;
  end;

  // Write into code source :
  if SourceCode <> ''
  then Writer := SourceEditor.CreateWriter
  else Writer := Nil;

  if Assigned(Writer)
  then begin
    UsesPos := Pos('USES', Uppercase(SourceCode));

    if UsesPos <> 0
    then begin
      UsesStr := '';
      Position := UsesPos;
      while SourceCode[Position] <> ';' do
      begin
        UsesStr := UsesStr + SourceCode[Position];
        Inc(Position, 1);
      end;
    end;

    if Pos(UpperCase(aUnitName), UpperCase(UsesStr)) = 0
    then begin
      Writer.CopyTo(UsesPos + Length(UsesStr) - 1);
      Writer.Insert(PAnsiChar(', ' + aUnitName));
    end;
  end;
end;  *)

end.
