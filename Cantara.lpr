program Cantara;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, SongSelection, Present, settings, info,
  lyrics,
  { you can add units after this }
  CommandLineOptions, CustApp, fgl;
{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  {if HasOption('h', 'help') then
  begin
    PrintHelp;
    Application.Free;
    Terminate;
    Exit;
  end; }
  Application.CreateForm(TfrmSongs, frmSongs);
  Application.CreateForm(TfrmPresent, frmPresent);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.Run;
end.

