{
  This is the main unit for the program Cantara. It imports all units used for
  implementing forms and starts the program.
  Many things are handled automatically by the Lazarus IDE, but some epecific changes have been made.
}
program Cantara;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  runtimetypeinfocontrols,
  lazcontrols,
  SongSelection,
  Present,
  settings,
  info,
  welcome,
  songeditor,
  { you can add units after this }
  slides,
  fulltextsearch,
  FormFulltextSearch,
  pptx,
  resourcehandling,
  PresentationCanvas,
  settingsdetailed,
  settingspadding,
  formMarkupExport,
  markup,
  imageexport,
  loadimagethread, CantaraStandardDialogs, presentationcontroller,
  cantarafontdialog, exporterinterfaces, thirdpartylibraries
  {$IFDEF WINDOWS}
  {
   On Windows, WinForms normally does not adjust to darkmode. We will use the Lazarus package MetaDarkStyle which sets the colors
   to dark style variants according to the system settings.
   However, that also means that we have to be careful when manually setting colors – don't hard code them if it is part of a
   component etc.
   Linux and Mac OS X doen't already support dark mode out of the box, so the extra package is not needed.
  }
  ,
  uDarkStyleParams,
  uMetaDarkStyle,
  uDarkStyleSchemes
  {$ENDIF}
  ;
  {$R *.res}

begin
  {$if declared(UseHeapTrace)}
  GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.2.0
  {$endIf}
  Application.Scaled:=True;
  {$IFDEF WINDOWS}
  { Only under Windows, we will set the darkmode according to the system settings. }
  RequireDerivedFormResource:=True;
  PreferredAppMode:=pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$ENDIF}
  Application.Initialize;

  Application.CreateForm(TfrmSongs, frmSongs);

  Application.CreateForm(TCFontDialog, CFontDialog);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.CreateForm(TfrmPresent, frmPresent);
  Application.CreateForm(TfrmSongEdit, frmSongEdit);
  Application.CreateForm(TfrmWelcome, frmWelcome);
  Application.CreateForm(TfrmWrapperFulltextSearch, frmWrapperFulltextSearch);
  Application.CreateForm(TFormPadding, FormPadding);
  Application.CreateForm(TFrmMarkupExport, FrmMarkupExport);
  Application.CreateForm(TFormImageExport, FormImageExport);
  Application.CreateForm(TThirdPartyLibrariesForm, ThirdPartyLibrariesForm);
  Application.Run;
end.
