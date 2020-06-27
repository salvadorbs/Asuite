unit VirtualFileSearch.Helper;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, VirtualTrees{, VirtualFileSearch};

type
  TVFSHelper = class helper for TVirtualFileSearch
  private
    function GetIsRunning(): Boolean;
  public
    property IsRunning: Boolean read GetIsRunning;
  end;

implementation

function TVFSHelper.GetIsRunning(): Boolean;
begin
  result := Assigned(FileFindThread);
end;

end.
