unit Frame.BaseEntity;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfrmBaseEntityPage = class(TFrame)
    OpenDialog1: TOpenDialog;
    procedure OpenDialog1Close(Sender: TObject);
  private
    { Private declarations }
  strict protected
    function GetTitle: string; virtual;
    function GetImageIndex: integer; virtual;
    function InternalLoadData: Boolean; virtual;
    function InternalSaveData: Boolean; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function SaveData: Boolean; virtual;
    property Title: string read GetTitle;
    property ImageIndex: integer read GetImageIndex;
  end;

TPageFrameClass = class of TfrmBaseEntityPage;

implementation

uses
  Kernel.Consts;

{$R *.dfm}

{ TfrmBaseEntityPage }

constructor TfrmBaseEntityPage.Create(AOwner: TComponent);
begin
  inherited;
  Self.InternalLoadData;
end;

function TfrmBaseEntityPage.GetImageIndex: integer;
begin
  Result := -1;
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

procedure TfrmBaseEntityPage.OpenDialog1Close(Sender: TObject);
begin
  SetCurrentDir(SUITE_WORKING_PATH);
end;

function TfrmBaseEntityPage.SaveData: Boolean;
begin
  Result := Self.InternalSaveData;
end;

end.
