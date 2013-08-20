unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfMain = class(TForm)
    bBrowseOutputFile: TButton;
    bGo: TButton;
    bProjectFileBrowse: TButton;
    cbRemoveFromProject: TCheckBox;
    eOutputFile: TEdit;
    eProjectFile: TEdit;
    lConversionLog: TLabel;
    lInfo: TLabel;
    lOutputFile: TLabel;
    lProjectFile: TLabel;
    mConversionLog: TMemo;
    odProjectFile: TOpenDialog;
    pMain: TPanel;
    sdOutputFile: TSaveDialog;
    procedure bBrowseOutputFileClick(Sender: TObject);
    procedure bGoClick(Sender: TObject);
    procedure bProjectFileBrowseClick(Sender: TObject);
    procedure eOutputFileChange(Sender: TObject);
    procedure eProjectFileChange(Sender: TObject);
  private
    procedure UpdateState;
  end;

var
  fMain: TfMain;

implementation
{$R *.dfm}
uses DKLang, DKL_ResFile;

  procedure TfMain.bBrowseOutputFileClick(Sender: TObject);
  begin
    sdOutputFile.FileName := eOutputFile.Text;
    if sdOutputFile.Execute then eOutputFile.Text := sdOutputFile.FileName;
  end;

  procedure TfMain.bGoClick(Sender: TObject);
  var
    sProjectFile, sOutputFile: String;
    ResFileIn, ResFileOut: TDKLang_ResFile;
    ConstEntryIn, ConstEntryOut: TDKLang_ResEntry;

    procedure LogString(const s: String); overload;
    begin
      mConversionLog.Lines.Add(s);
    end;

    procedure LogString(const s: String; const aParams: Array of const); overload;
    begin
      LogString(Format(s, aParams));
    end;

  begin
    try
       // Check prerequisites
      sProjectFile  := Trim(eProjectFile.Text);
      sOutputFile := Trim(eOutputFile.Text);
      LogString('Project file: %s', [sProjectFile]);
      LogString('Output file: %s',  [sOutputFile]);
      if AnsiSameText(sProjectFile, sOutputFile) then raise Exception.Create('You shoudn''t specify the same file for both project and output files');
       // Try to load the file
      ResFileIn := TDKLang_ResFile.Create;
      try
        ResFileIn.LoadFromFile(sProjectFile);
        LogString('Project file loaded successfully.');
         // Look for constant resource
        ConstEntryIn := ResFileIn.FindEntry(IntToStr(Integer(RT_RCDATA)), SDKLang_ConstResourceName);
        if ConstEntryIn=nil then
          LogString('The file doesn''t contain constant resource ("%s"), no need to convert it.', [SDKLang_ConstResourceName])
        else begin
          LogString('Constant resource "%s" found (%d bytes data).', [SDKLang_ConstResourceName, Length(ConstEntryIn.RawData)]);
           // Create output file
          ResFileOut := TDKLang_ResFile.Create;
          try
             // Copy the resource
            ConstEntryOut := ConstEntryIn.Clone;
            ResFileOut.AddEntry(ConstEntryOut);
            LogString('Constant entry cloned.');
             // Save the file
            ResFileOut.SaveToFile(sOutputFile);
            LogString('Output resource file saved successfully.');
          finally
            ResFileOut.Free;
          end;
           // Remove the entry from the input file, if needed
          if cbRemoveFromProject.Checked then begin
            ResFileIn.RemoveEntry(ConstEntryIn);
            LogString('Constant entry removed from the project file.');
             // Save the project resource file
            ResFileIn.SaveToFile(sProjectFile);
            LogString('Project resource file saved successfully.');
          end;
        end;
      finally
        ResFileIn.Free;
      end;
      LogString('Finished.');
      LogString('----------------------------------------');
    except
      on e: Exception do begin
        LogString('Exception class: %s',   [e.ClassName]);
        LogString('Exception message: %s', [e.Message]);
        raise;
      end;
    end;
  end;

  procedure TfMain.bProjectFileBrowseClick(Sender: TObject);
  begin
    odProjectFile.FileName := eProjectFile.Text;
    if odProjectFile.Execute then eProjectFile.Text := odProjectFile.FileName;
  end;

  procedure TfMain.eOutputFileChange(Sender: TObject);
  begin
    UpdateState;
  end;

  procedure TfMain.eProjectFileChange(Sender: TObject);
  begin
    eOutputFile.Text := ChangeFileExt(eProjectFile.Text, '.dkl_const.res');
    UpdateState;
  end;

  procedure TfMain.UpdateState;
  begin
    bGo.Enabled := (eProjectFile.Text<>'') and (eOutputFile.Text<>'');
  end;

end.
