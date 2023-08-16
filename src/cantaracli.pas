program cantaracli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  Lyrics, Slides;
type

  { TCantaraCLI }

  TCantaraCLI = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TCantaraCLI }

procedure TCantaraCLI.DoRun;
var
  ErrorMsg, SplittedText: String;
  Song: TSong;
  SlideLIst: TSlideList;
begin
  // quick check parameters
  { ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;         }

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if ParamCount <= 0 then
  begin
    Terminate;
    Exit;
  end;
  { add your program here }
  if not FileExists(ParamStr(1)) then
  begin
    WriteLn('The file ', ParamStr(1), ' could not be found.');
    Terminate(1);
    Exit;
  end;
  WriteLn('Hello');
  WriteLn('%s', ParamStr(1));

  Song := TSong.Create;
  Song.importSongfile(ParamStr(1));
  SplittedText := SplitSlides(Song.output, 2);
  WriteLn(SplittedText);

  // stop program loop
  Song.Destroy;
  Terminate;
end;

constructor TCantaraCLI.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCantaraCLI.Destroy;
begin
  inherited Destroy;
end;

procedure TCantaraCLI.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TCantaraCLI;
begin
  Application:=TCantaraCLI.Create(nil);
  Application.Title:='cantara-cli';
  Application.Run;
  Application.Free;
end.

