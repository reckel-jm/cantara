unit lyrics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Strings, fgl;

function StringListToString(StringList: TStringList): String;
type
  TStringDict = specialize TFPGMap<string, string>;
  TStringIntegerDict = specialize TFPGMap<string, integer>;
  { TSong }
  TSong = Class
    public
      filename: String;
      output: TStringList;
      MetaDict: TStringDict;
      delimCounter: Integer;
      constructor Create; overload;
      destructor Destroy; override;
      procedure importSongFile;
      procedure importSongfile(filepath: string);
      procedure ConvertFile;
    private
      inputFile: TStringList;
      PositionDict: TStringIntegerDict;
      procedure WritePart(index: Integer);
      procedure IncludeRepetitionalParts;
      procedure importSongLegacyFile;
      procedure importCCLISongFile;
      procedure importCCLISongFile(filepath: string);
  end;


implementation


{ Create all Lists and Dictionaries with the Constructor }
constructor TSong.Create;
begin
  inherited;
  self.output := TStringList.Create;
  self.MetaDict := TStringDict.Create;
  self.inputFile := TStringList.Create;
  self.PositionDict := TStringIntegerDict.Create;
end;

{ Destroy all Lists and Dictionaries with the Destuctor }
destructor TSong.Destroy;
begin
  self.output.Free;
  self.MetaDict.Free;
  self.inputFile.Free;
  self.PositionDict.Free;
  inherited;
end;

procedure TSong.importCCLISongFile;
begin
  self.inputFile.LoadFromFile(self.filename);
  ConvertFile;
end;

procedure TSong.importCCLISongFile(filepath: String);
begin
  self.filename := filepath;
  self.importCCLISongFile;
end;

procedure TSong.ConvertFile;
var i: Integer;
  j: Integer;
  RefrainState: Boolean;
begin
  self.MetaDict.Add('Title', inputFile.Strings[0]);
  RefrainState := False;
  for i :=  1 to self.inputFile.Count-1 do
  begin
    { The Parts Chorus and PreChorus are repeated after every other part (stanza+bridge). So, there position should be remembered. }
    if (pos('Chorus', self.inputFile.Strings[i]) = 1) or (self.inputFile.Strings[i] = 'PreChorus') then
    begin
      self.PositionDict.Add(self.inputFile.Strings[i],i); { Add the Element Name and the line number }
      WritePart(i);
      RefrainState := False;
    end else
    { The Parts Vers/Strophe and Bridge normally do not get repeated. They are only used once at the printed position. However, after them, the repetitional parts should follow. }
    if (pos('Strophe ',inputFile.Strings[i]) = 1) or (pos('Vers ',inputFile.Strings[i]) = 1) or (inputFile.Strings[i] = 'Vers') or (pos('Bridge',inputFile.Strings[i]) = 1) then
    begin
      if RefrainState = True then
         IncludeRepetitionalParts;
      WritePart(i);
      RefrainState := True;
    end else
    { Handle the CCLI Copyright information }
    if (pos('CCLI', self.inputFile.Strings[i]) = 1) then
    begin
      if i < self.inputFile.Count-1 then
      begin
        self.MetaDict.Add('CCLI-Songnumber', self.inputFile.Strings[i].Split(' ')[1]);
        self.MetaDict.Add('Author',self.inputFile.Strings[i+1]);
      end else
      begin
        self.MetaDict.Add('CCLI-Licence', self.inputFile.Strings[i].Split(' ')[1]);
      end;
    end;
  end;
  { Add Closing Refrain if needed }
  if RefrainState = True then
    IncludeRepetitionalParts;

  { Remove the last lines, if they are blank }
  While self.Output.Strings[self.Output.Count-1] = '' do
  begin
    self.Output.Delete(self.Output.Count-1);
  end;
end;

procedure TSong.WritePart(index: Integer);
var i,length: Integer;
begin
  i := index + 1;
  while (self.inputFile.Strings[i] <> '') and (i <= self.inputFile.Count-1) do
  begin
    self.output.Add(self.inputFile[i]); // Copy the Line
    inc(i);
  end;
  length := i - index - 1;
  if length >= self.delimCounter then { seperate the parts which are two big in two peaces }
  begin
    self.output.Insert(self.output.count-1-(length div 2)+1,'');
  end;
  self.output.Add(''); // An empty line at the end of a song part
end;

procedure TSong.IncludeRepetitionalParts;
var i, key: Integer;
  foundChorus: Boolean;
begin
  { Find the latest Chorus, print it as well }
  foundChorus := False;
  for i := 0 to self.PositionDict.Count-1 do
  begin
    if self.PositionDict.Keys[i] = 'PreChorus' then WritePart(self.PositionDict.Data[i]);
    if pos('Chorus',self.PositionDict.Keys[i]) > 0 then
    begin
      key := self.PositionDict.Data[i]; foundChorus := True;
    end;
  end;
  if foundChorus then WritePart(key);
end;

function StringListToString(StringList: TStringList): String;
var s: String; i: integer;
begin
  s := '';
  if StringList.Count < 1 then exit(s);
  for i := 0 to StringList.Count-2 do
  begin
    s := s + StringList.Strings[i] + LineEnding;
  end;
  Result := s;
end;

procedure TSong.importSongLegacyFile;
begin
  self.inputFile.LoadFromFile(self.filename);
  self.output.Assign(self.inputFile);
end;

{ This function finds out which format the song has and calls the specific import function }
procedure TSong.importSongFile;
var songfileextension: String;
begin
  songfileextension := ExtractFileExt(self.filename);
  if songfileextension = '.song' then
    self.importSongLegacyFile
  else if (songfileextension = '.txt') or (songfileextension = '.ccli') then // CCLI-Songselect file
    self.importCCLISongFile;
end;

procedure TSong.importSongFile(filepath: String);
begin
  self.filename:=filepath;
  importSongFile;
end;

end.

