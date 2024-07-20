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
  cantarafontdialog, exporterinterfaces
  {$IFDEF WINDOWS}
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
  Application.Run;
end.
