unit ufrFontSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DKLang, StdCtrls, ExtCtrls;

type
  TfrFontSettings = class(TFrame)
    bSelectFont: TButton;
    gbMain: TGroupBox;
    lcMain: TDKLanguageController;
    pSample: TPanel;
    procedure bSelectFontClick(Sender: TObject);
  private
     // Prop handlers
    function  GetSelectedFont: TFont;
    function  GetTitle: UnicodeString;
    procedure SetSelectedFont(Value: TFont);
    procedure SetTitle(const Value: UnicodeString);
  public
     // Props
     // -- Frame title, assigned at runtime (we cannot localize it at design time since all of the controllers share the
     //    same translation in this example)
    property Title: UnicodeString read GetTitle write SetTitle;
     // -- A font selected in the editor
    property SelectedFont: TFont read GetSelectedFont write SetSelectedFont;
  end;

implementation
{$R *.dfm}

  procedure TfrFontSettings.bSelectFontClick(Sender: TObject);
  var fd: TFontDialog;
  begin
    fd := TFontDialog.Create(Self);
    try
      fd.Font.Assign(SelectedFont);
      if fd.Execute then SelectedFont := fd.Font;
    finally
      fd.Free;
    end;
  end;

  function TfrFontSettings.GetSelectedFont: TFont;
  begin
    Result := pSample.Font;
  end;

  function TfrFontSettings.GetTitle: UnicodeString;
  begin
    Result := gbMain.Caption;
  end;

  procedure TfrFontSettings.SetSelectedFont(Value: TFont);
  begin
    pSample.Font.Assign(Value);
  end;

  procedure TfrFontSettings.SetTitle(const Value: UnicodeString);
  begin
    gbMain.Caption := Value;
  end;

end.

