{
Copyright (C) 2006-2020 Matteo Salvi

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

unit Scoop.Wrapper;

interface

uses
  classes, PJPipe, PJConsoleApp, PJPipeFilters, PJFileHandle, SysUtils;

const
  SCOOP_CMD = 'cmd.exe /c scoop';

type

  TScoopWrapper = class
  private
    { private declarations }
    FApp: TPJConsoleApp;
    FOutput: TStringList;
    FOutPipe: TPJPipe;
    FErrPipe: TPJPipe;
    FOutFilter: TPJAnsiSBCSPipeFilter;

    procedure LineEndHandler(Sender: TObject; const Text: AnsiString);
    procedure WorkHandler(Sender: TObject);
    procedure CompleteHandler(Sender: TObject);
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(); virtual;
    destructor Destroy(); 

    function IsExists(): boolean;
    function GetScoopVersion(): string;
    procedure Update();
    procedure InstallApp();
    procedure UninstallApp();
    procedure Status();
  published
    { published declarations }
  end;

implementation

{ TScoopWrapper }

constructor TScoopWrapper.Create();
begin
  inherited;
  FApp := TPJConsoleApp.Create; 
  FOutPipe := TPJPipe.Create;
  FErrPipe := TPJPipe.Create; 
  FOutput := TStringList.Create;
  FOutFilter := TPJAnsiSBCSPipeFilter.Create(FOutPipe, True);

  FOutFilter.OnLineEnd := LineEndHandler;

  FApp.MaxExecTime := INFINITE;
  FApp.TimeSlice := 5;
  FApp.Visible := True;
  FApp.StdOut := FOutPipe.WriteHandle;
  FApp.StdErr := FErrPipe.WriteHandle;
  FApp.OnWork := WorkHandler;
  FApp.OnComplete := CompleteHandler;
end;

destructor TScoopWrapper.Destroy();
begin
  FApp.Free;   
  FOutPipe.Free;
  FErrPipe.Free;
  FreeAndNil(FOutFilter);
end;

function TScoopWrapper.GetScoopVersion: string;
begin

end;

procedure TScoopWrapper.InstallApp;
begin

end;

function TScoopWrapper.IsExists(): boolean;
var
  isExecuted: Boolean;
begin
  try
    FOutput.Clear;
    
    isExecuted := FApp.Execute(SCOOP_CMD + ' --version');
    Result := (isExecuted) and (FOutput.Count > 0);

  except
    raise;
  end;
end;

procedure TScoopWrapper.WorkHandler(Sender: TObject);
begin
  FOutFilter.ReadPipe;
end;

procedure TScoopWrapper.LineEndHandler(Sender: TObject; const Text: AnsiString);
begin
  FOutput.Add(Text);
end;      

procedure TScoopWrapper.Status;
begin

end;

procedure TScoopWrapper.UninstallApp;
begin

end;

procedure TScoopWrapper.Update;
begin

end;

procedure TScoopWrapper.CompleteHandler(Sender: TObject);
begin
  FOutFilter.Flush;
end;

end.
