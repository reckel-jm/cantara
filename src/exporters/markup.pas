unit markup;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, FGL, Lyrics;

type

  TPartDict = specialize TFPGMap<string, string>;

  TMarkupExporter = class
  private
    Output: TStringList;
    PartDict: TPartDict;
    SongList: TSongList;
    Header: String;
    Body: String;
    Footer: String;
    procedure HandlePart(PartTitle: String; PartContent: String);
    procedure HandleSongloopPart;
    function HandleSongLoopCommand(command: String; Song: TSong): String;
    function RunNormalCommands(Part: String): String;
    function RunNormalCommand(Command: String): String;
  public
    FileExtension: String;
    constructor Create(Template: String; ASongList: TSongList); overload;
    destructor Destroy; override;
    property ParsingOutput: TStringList read Output;
  end;

const
  PARTREGEX:String = '\\(\w{1,})\s*\{([^\}]*)\}';
  COMMANDREGEX:String = '\\(\w{1,})';

implementation

constructor TMarkupExporter.Create(Template: String; ASongList: TSongList);
var regex: TRegExpr;
begin
  inherited Create;
  Output := TStringList.Create;
  SongList := ASongList;
  PartDict := TPartDict.Create;
  PartDict.Add('header', '');
  PartDict.Add('songloop', '');
  PartDict.Add('footer', '');
  PartDict.Add('linebeginning', '');
  PartDict.Add('lineending', '');
  PartDict.Add('betweensongs', LineEnding + LineEnding);
  PartDict.Add('linebreak', LineEnding);
  PartDict.Add('delimiter', LineEnding + LineEnding);
  regex := TRegExpr.Create(PARTREGEX);
  if regex.Exec(Template) then
  begin
    HandlePart(regex.Match[1], regex.Match[2]);
    while (regex.ExecNext) do
    begin
      HandlePart(regex.Match[1], regex.Match[2]);
    end;
  end;
  regex.Destroy;
  //self.Output.Text := StringReplace(self.Output.Text, '\newline', LineEnding, [rfReplaceAll]);
  self.Output.Text := Trim(Trim(self.Header) + PartDict['delimiter'] + Trim(self.Body));
  if self.Footer <> '' then self.output.Text := self.Output.Text + PartDict['delimiter'] + Trim(self.Footer);
end;

procedure TMarkupExporter.HandlePart(PartTitle: String; PartContent: String);
begin
  if PartDict.IndexOf(PartTitle) >= 0 then
  begin
    PartDict[PartTitle] := PartContent;
    case PartTitle of
      'songloop' : HandleSongloopPart;
      'header' : self.Header:=RunNormalCommands(PartContent);
      'footer' : self.Footer:=RunNormalCommands(PartContent);
      'fileextension': self.FileExtension:=PartContent;
    else PartDict[PartTitle] := RunNormalCommands(PartContent);
    end;
  end else
    PartDict.Add(PartTitle, RunNormalCommands(PartContent));
end;

procedure TMarkupExporter.HandleSongloopPart;
var Part, GeneratedContent: String;
  i: Integer;
  regex: TRegExpr;
  Song: TSong;
  CommandList: TStringList;
  Command: String;
begin
  CommandList := TStringList.Create;
  Part := PartDict['songloop'];
  regex := TRegExpr.Create(COMMANDREGEX);
  if regex.Exec(Part) then
  begin
    CommandList.Add(regex.Match[1]);
    while (regex.ExecNext) do
    begin
      CommandList.Add(regex.Match[1]);
    end;
  end;
  Regex.Destroy;
  for i := 0 to SongList.Count-1 do
  begin
    Song := SongList.Items[i];
    GeneratedContent := Part;
    for Command in CommandList do
    begin
      GeneratedContent := StringReplace(GeneratedContent, '\' + Command, HandleSongLoopCommand(Command, Song), [rfReplaceAll]);
    end;
    body += Trim(GeneratedContent);
    if i < SongList.Count-1 then body += PartDict['betweensongs'];
  end;
  CommandList.Destroy;
end;

function TMarkupExporter.RunNormalCommands(Part: String): String;
var regex: TRegExpr;
begin
  regex := TRegExpr.Create(COMMANDREGEX);
  if regex.Exec(Part) then
  begin
    Part := StringReplace(Part, '\' + regex.Match[1], RunNormalCommand(regex.Match[1]), [rfReplaceAll]);
    while (regex.ExecNext) do
    begin
      Part := StringReplace(Part, '\' + regex.Match[1], RunNormalCommand(regex.Match[1]), [rfReplaceAll]);
    end;
  end;
  Regex.Destroy;
  Result := Part;
end;

function TMarkupExporter.RunNormalCommand(Command: String): String;
begin
  if (command = 'newline') or (command = 'linebreak') then
     Result := LineEnding
  else Result := '';
end;

function TMarkupExporter.HandleSongLoopCommand(command: String; Song: TSong): String;
var i: Integer;
  line: String;
  CopiedSongOutput: TStringList;
begin
  if Song.MetaDict.IndexOf(lowercase(command)) >= 0 then
     Result := Song.MetaDict[lowercase(command)]
  else if command = 'lyrics' then
  begin
    CopiedSongOutput := TStringList.Create;
    CopiedSongOutput.Assign(Song.Output);
    for i := 0 to CopiedSongOutput.Count-1 do
    begin
      line := CopiedSongOutput.Strings[i];
      line := Partdict['linebeginning'] + line + Partdict['lineending'];
      CopiedSongOutput.Strings[i] := line;
    end;
    Result := CopiedSongOutput.Text;
    CopiedSongOutput.Destroy;
  end
  else if (command = 'newline') or (command = 'linebreak') then
     Result := LineEnding
  else
    Result := '';
end;

destructor TMarkupExporter.Destroy;
begin
  Output.Destroy;
  PartDict.Destroy;
  inherited;
end;

end.

