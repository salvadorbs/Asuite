program dbg2Mab;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  mormot.core.log,
  mormot.core.text;

const
  {$IFDEF MSWINDOWS}
  EXE_CONST = '.exe';
  {$ELSE}
  EXE_CONST = '';
  {$ENDIF}

procedure Process(const FileName: TFileName);
var
   Ext: integer;
   AllOk: boolean;
begin
  AllOk := True;

  Ext := GetFileNameExtIndex(FileName, 'map,dbg');
  if (Ext>=0) and (FileExists(FileName)) then
  begin
    WriteLn('Executable file: ' + ChangeFileExt(FileName, EXE_CONST));
    WriteLn('Debug file: ' + FileName);
    try
      with TSynMapFile.Create(FileName, true) do // true = .map/.dbg -> .mab
      try
        if not HasDebugInfo then
        begin
          WriteLn('Error: no Debug Info found on ', FileName);
          AllOk := False;
        end
        else begin
          // has debug info
          WriteLn('Embedded debug info to file');
          SaveToExe(ChangeFileExt(FileName, EXE_CONST));
        end;
      finally
        Free;
      end;
    except
      on E: Exception do begin // ignore any problem here: just print it and process next file
        WriteLn('Error: ', E.ClassName,' ',E.Message);
        AllOk := False;
      end;
    end;
  end;

  if not AllOk then
    ExitCode := 1;
end;

begin
  if paramCount>0 then
    Process(paramstr(1));
end.
