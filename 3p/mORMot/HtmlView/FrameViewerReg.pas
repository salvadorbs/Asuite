unit FrameViewerReg;

interface

uses classes, HTMLView, FramView, FramBrwz;

procedure Register;

{$R FramView.dcr}
{$R HtmlView.dcr}
{$R FramBrwz.dcr}

implementation

procedure Register;
begin
  RegisterComponents('Standard', [THTMLViewer, TFrameViewer, TFrameBrowser]);
end;

end.
