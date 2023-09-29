program Cantara;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, lazcontrols, SongSelection, Present, settings,
  info, welcome, songeditor,
  { you can add units after this }
  slides, fulltextsearch, FormFulltextSearch, pptx, resourcehandling,
  PresentationCanvas, settingsdetailed, settingspadding, formMarkupExport,
  markup, imageexport, loadimagethread, presentationcontroller;
{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmSongs, frmSongs);
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

