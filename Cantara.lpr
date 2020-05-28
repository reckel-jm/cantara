program Cantara;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, Unit1, Present, settings, info
  { you can add units after this };

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmSongs, frmSongs);
  Application.CreateForm(TfrmPresent, frmPresent);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.Run;
end.

