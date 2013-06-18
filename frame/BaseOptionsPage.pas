unit BaseOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfrmBaseOptionsPage = class(TFrame)
  private
    { Private declarations }
  strict protected
    function GetTitle: string; virtual;
  public
    { Public declarations }
    property Title: string read GetTitle;
  end;

TPageFrameClass = class of TfrmBaseOptionsPage;

implementation

{$R *.dfm}

{ TfrmBaseOptionsPage }

function TfrmBaseOptionsPage.GetTitle: string;
begin
  Result := '';
end;

end.
