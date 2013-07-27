unit BaseEntityPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfrmBaseEntityPage = class(TFrame)
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

TPageFrameClass = class of TfrmBaseEntityPage;

implementation

{$R *.dfm}

{ TfrmBaseEntityPage }

constructor TfrmBaseEntityPage.Create(AOwner: TComponent);
begin
  inherited;
  Self.InternalLoadData;
end;

function TfrmBaseEntityPage.GetTitle: string;
begin
  Result := '';
end;

function TfrmBaseEntityPage.InternalLoadData: Boolean;
begin
  Result := True;
end;

function TfrmBaseEntityPage.InternalSaveData: Boolean;
begin
  Result := True;
end;

procedure TfrmBaseEntityPage.SaveData;
begin
  Self.InternalSaveData;
end;

end.