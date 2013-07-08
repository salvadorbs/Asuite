unit BaseOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfrmBaseOptionsPage = class(TFrame)
    OpenDialog1: TOpenDialog;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; virtual;
    function InternalLoadData: Boolean; virtual;
    function InternalSaveData: Boolean; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SaveData; virtual;
    property Title: string read GetTitle;
  end;

TPageFrameClass = class of TfrmBaseOptionsPage;

implementation

{$R *.dfm}

{ TfrmBaseOptionsPage }

constructor TfrmBaseOptionsPage.Create(AOwner: TComponent);
begin
  inherited;
  Self.InternalLoadData;
end;

function TfrmBaseOptionsPage.GetTitle: string;
begin
  Result := '';
end;

function TfrmBaseOptionsPage.InternalLoadData: Boolean;
begin
  Result := True;
end;

function TfrmBaseOptionsPage.InternalSaveData: Boolean;
begin
  Result := True;
end;

procedure TfrmBaseOptionsPage.SaveData;
begin
  Self.InternalSaveData;
end;

end.
