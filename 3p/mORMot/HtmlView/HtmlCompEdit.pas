{Version 10.00}

{$I htmlcons.inc}

unit HTMLCompEdit;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, StdCtrls,
  Dialogs, ExtCtrls,
{$IFDEF Delphi6_Plus}
  designintf, DesignEditors;
{$ELSE}
  dsgnintf;
{$ENDIF}


type

  THtComponentEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(index: Integer): string; override;
    procedure ExecuteVerb(index: Integer); override;
  end;

  THTMLEditor = class(THtComponentEditor)
  end;

  TFMVEditor = class(THtComponentEditor)
  end;

  TFMBEditor = class(THtComponentEditor)
  end;

procedure Register;

implementation

uses
  htmlview, htmlun2, framview, frambrwz;

procedure Register;
begin
  RegisterComponentEditor(THTMLViewer, THTMLEditor);
  RegisterComponentEditor(TFrameViewer, TFMVEditor);
  RegisterComponentEditor(TFrameBrowser, TFMBEditor);
end;

{ THtComponentEditor }

function THtComponentEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function THtComponentEditor.GetVerb(index: Integer): string;
begin
  Result := 'About..';
end;

procedure THtComponentEditor.ExecuteVerb(index: integer);
begin
  MessageDlg(
    GetComponent.ClassName + ', Version (V) ' + VersionNo + #13#13 +
    'Copyright (C) 1995-2008 by L. David Baldwin'#13#13 +
    'Copyright (C) 2008-2009 by the HtmlViewer Project Team'#13#13 +
    '        Patrick van Logchem'#13 +
    '        Sebastian Zierer'#13 +
    '        Arvid Winkelsdorf'#13 +
    '        Bernd Gabriel',
    mtInformation, [mbOk], 0);
end;

end.
