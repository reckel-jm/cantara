program Cantara;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, SongSelection, Present, settings,
  info,
  { you can add units after this }
  songeditor, welcome;
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
  Application.Run;
end.

