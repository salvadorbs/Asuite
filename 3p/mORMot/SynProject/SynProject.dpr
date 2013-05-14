/// main SynProject program file
// - this unit is part of SynProject, under GPL 3.0 license; version 1.13
program SynProject;

(*
    This file is part of SynProject.

    Synopse SynProject. Copyright (C) 2008-2011 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    SynProject is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or (at
    your option) any later version.

    SynProject is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with SynProject. If not, see <http://www.gnu.org/licenses/>.


    Compilation hint:
     - tested under Delphi 6/7 ONLY - should work with Delphi 2007, but would
       NOT work with Delphi 2009/2010/XE (this source code is not Unicode-Ready)
     - you should have entered .\PasDocLight in your Project/Options/Directories
       SearchPath field
     - you should have added the TMemoEx component (from SynMemoEx.pas) to the IDE
     - and of course, the synopse Lib directory must be in your Tools/Environment
       Options/Libray/Library Path
     
*)

uses
  FastMM4,
  Forms,
  ProjectVersionMain in 'ProjectVersionMain.pas' {MainForm},
  ProjectVersioning in 'ProjectVersioning.pas',
  ProjectSections in 'ProjectSections.pas',
  ProjectVersionPages in 'ProjectVersionPages.pas' {FramePages: TFrame},
  ProjectFrameViewer in 'ProjectFrameViewer.pas' {FrameViewer: TFrame},
  ProjectMemoExSyntax in 'ProjectMemoExSyntax.pas',
  ProjectFormViewOne in 'ProjectFormViewOne.pas' {FormViewOne},
  ProjectFormViewTwo in 'ProjectFormViewTwo.pas' {FormViewTwo},
  ProjectEditor in 'ProjectEditor.pas' {FrameEditor: TFrame},
  ProjectVersionSCR in 'ProjectVersionSCR.pas' {ProjectVersionSCRForm},
  ProjectVersionBackup in 'ProjectVersionBackup.pas' {ProjectVersionBackupForm},
  ProjectFrameRisk in 'ProjectFrameRisk.pas' {FrameRisk: TFrame},
  ProjectVersionCommit in 'ProjectVersionCommit.pas' {ProjectVersionCommitForm},
  ProjectEditMain in 'ProjectEditMain.pas' {ProMainForm},
  ProjectVersionCompare in 'ProjectVersionCompare.pas' {ProjectVersionCompareForm},
  ProjectFormDocWizard in 'ProjectFormDocWizard.pas' {ProjectDocWizard},
  ProjectEditorRelease in 'ProjectEditorRelease.pas' {ProjectEditorReleaseForm},
  ProjectTypes in 'ProjectTypes.pas',
  ProjectTrackerLogin in 'ProjectTrackerLogin.pas' {ProjectTrackerLoginForm},
  ProjectEditorCommit in 'ProjectEditorCommit.pas' {ProjectEditorCommitForm},
  ProjectGraphEdit in 'ProjectGraphEdit.pas' {GraphEditForm},
  ProjectSpellCheck in 'ProjectSpellCheck.pas' {SpellCheckForm},
  ProjectEditorProgram in 'ProjectEditorProgram.pas' {ProjectEditorProgramForm},
  ProjectCommons in 'ProjectCommons.pas',
  ProjectDiff in 'ProjectDiff.pas',
  ProjectDiffUnit in 'ProjectDiffUnit.pas',
  ProjectParser in 'ProjectParser.pas',
  ProjectRTF in 'ProjectRTF.pas',
  ProjectTrkTool in 'ProjectTrkTool.pas',
  ProjectFormSelection in 'ProjectFormSelection.pas' {SelectionForm},
  ProjectDiagrams in 'ProjectDiagrams.pas';

{$R *.res}
{$R Vista.res} 

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormViewOne, FormViewOne);
  Application.CreateForm(TFormViewTwo, FormViewTwo);
  Application.CreateForm(TProjectVersionSCRForm, ProjectVersionSCRForm);
  Application.CreateForm(TProjectVersionBackupForm, ProjectVersionBackupForm);
  Application.CreateForm(TProjectVersionCommitForm, ProjectVersionCommitForm);
  Application.CreateForm(TProMainForm, ProMainForm);
  Application.CreateForm(TProjectVersionCompareForm, ProjectVersionCompareForm);
  Application.CreateForm(TProjectDocWizard, ProjectDocWizard);
  Application.CreateForm(TProjectTrackerLoginForm, ProjectTrackerLoginForm);
  Application.Run;
end.
