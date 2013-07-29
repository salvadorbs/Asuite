unit BasePropertyPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ulNodeDataTypes, BaseEntityPage;

type
  TfrmBasePropertyPage = class(TfrmBaseEntityPage)
  private
    { Private declarations }
    FCurrentNodeData: TvCustomRealNodeData;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; ListNodeData: TvCustomRealNodeData); overload;
    property CurrentNodeData: TvCustomRealNodeData read FCurrentNodeData write FCurrentNodeData;
  end;

var
  frmBasePropertyPage: TfrmBasePropertyPage;

implementation

{$R *.dfm}

{ TfrmBasePropertyPage }

constructor TfrmBasePropertyPage.Create(AOwner: TComponent;
  ListNodeData: TvCustomRealNodeData);
begin
  Self.FCurrentNodeData := ListNodeData;
  inherited Create(AOwner);
end;

end.
