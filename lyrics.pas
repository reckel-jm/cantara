unit lyrics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Strings, fgl, Dialogs;

function StringListToString(StringList: TStringList): String;

type
  TStringDict = specialize TFPGMap<string, string>;
  TStringIntegerDict = specialize TFPGMap<string, integer>;
  TRepoFile = class
    Name: string;
    filePath: string;
  end;
  TRepoArray = array of TRepoFile;

  { TSong }
  TSong = Class
    public
      filename: String;
      output: TStringList;
      MetaDict: TStringDict; { contains all the MetaData of the songs }
      MaxSlideLineLength: Integer; { When should slices be cut into two parts }
      constructor Create; overload;
      destructor Destroy; override;
      procedure importSongFile;
      procedure importSongfile(filepath: string);
      procedure ConvertCCLIFile;
      procedure slideWrap;
      function ParseMetaData(MetaLogic: string): string;
    private
      inputFile: TStringList;
      PositionDict: TStringIntegerDict;
      procedure WritePart(index: Integer);
      procedure IncludeRepetitionalParts;
      procedure importSongFormatFile;
      procedure importCCLISongFile;
      procedure importCCLISongFile(filepath: string);
      function ParseMetaData(MetaLogic: string; count: integer): string;
  end;
  TSongList = specialize TFPGObjectList<TSong>;


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
  ConvertCCLIFile;
end;

procedure TSong.importCCLISongFile(filepath: String);
begin
  self.filename := filepath;
  self.importCCLISongFile;
end;

procedure TSong.ConvertCCLIFile;
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
        self.MetaDict.Add('ccli-songnumber', self.inputFile.Strings[i].Split(' ')[1]);
        self.MetaDict.Add('author',self.inputFile.Strings[i+1]);
      end else
      begin
        self.MetaDict.Add('ccli-licensenumber', self.inputFile.Strings[i].Split(' ')[1]);
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
  {
  This is not needed anymore, for the function slideWrap handles it in a universal way.
  if length >= self.MaxSlideLineLength then { seperate the parts which are two big in two peaces }
  begin
    self.output.Insert(self.output.count-1-(length div 2)+1,'');
  end; }
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

procedure TSong.importSongFormatFile;
var i: integer;
  curLineText, key, value: String;
  contentStarted: boolean;
begin
  self.inputFile.LoadFromFile(self.filename);
  contentStarted := False;
  for i := 0 to self.inputFile.Count-1 do
  begin
    curLineText := self.inputFile.Strings[i];
    if pos('#', curLineText) = 1 then
    begin
      if pos(':', curLineText) > 1 then
      key := curLineText.Split(':')[0];
      Delete(key, 1, 1); // Delete the # at the beginning
      value := curLineText.Split(':')[1];
      { Remove Whitespaces
        If none of the parts are empty, add the key-value-pair to the MetaData dictionary }
      if (key <> '') and (value <> '') then
        begin
          key := trim(key);
          value := trim(value);
        end;
      self.MetaDict.Add(lowerCase(key), value);
    end else
    if (Trim(curLineText) = '') and (contentStarted = True) then output.Add(curLineText)
    else if (Trim(curLineText) <> '') then
    begin
      output.Add(curLineText);
      contentStarted := True;
    end;
  end;

  //self.output.Assign(self.inputFile);
end;

procedure TSong.slideWrap;
var n1, n2,i: integer;
  changed: boolean;
begin
  repeat
  begin
  changed := False;
  if self.MaxSlideLineLength = 0 then exit; // Just as a protective measure, actually not needed anymore.
  n1 := 0;
  n2 := 0;
  for i := 0 to output.Count-1 do
  begin
    if output.Strings[i] = '' then
    begin
       n2 := i;
       if (n2-n1) > self.MaxSlideLineLength then
         begin
           if self.MaxSlideLineLength mod 2 = 0 then
             output.Insert((n1+(n2-n1) div 2), '')
           else output.Insert((n1+(n2-n1) div 2) + 1, '');
           changed := True;
         end;
       n1 := n2+1;
    end;
  end;
  // For the last slide
  n2 := output.Count;
  if (n2-n1) > self.MaxSlideLineLength then
     begin
         if self.MaxSlideLineLength mod 2 = 0 then
           output.Insert((n1+(n2-n1) div 2), '')
         else output.Insert((n1+(n2-n1) div 2) + 1, '');
         changed := True;
     end;
  end until changed = False;
end;

{ This function finds out which format the song has and calls the specific import function }
procedure TSong.importSongFile;
var songfileextension: String;
begin
  songfileextension := ExtractFileExt(self.filename);
  if songfileextension = '.song' then
    self.importSongFormatFile
  else if (songfileextension = '.txt') or (songfileextension = '.ccli') then // CCLI-Songselect file
    self.importCCLISongFile;
  if self.MaxSlideLineLength>0 then self.slideWrap;
end;

procedure TSong.importSongFile(filepath: String);
begin
  self.filename:=filepath;
  importSongFile;
end;

function TSong.ParseMetaData(MetaLogic: string): string;
var ParseString: String;
begin
  { Prepare String for Syntax }
  MetaLogic := StringReplace(MetaLogic, LineEnding, ' ' + LineEnding + ' ', [rfReplaceAll, rfIgnoreCase]);
  MetaLogic := StringReplace(MetaLogic, '{', ' {', [rfReplaceAll]);
  MetaLogic := StringReplace(MetaLogic, '}', '} ', [rfReplaceAll]);
  MetaLogic := StringReplace(MetaLogic, '{ ', '{', [rfReplaceAll]);
  MetaLogic := StringReplace(MetaLogic, ' }', '}', [rfReplaceAll]);
  MetaLogic := StringReplace(MetaLogic, '{% ', '{%', [rfReplaceAll]);
  MetaLogic := StringReplace(MetaLogic, ' %}', '%}', [rfReplaceAll]);
  { The Magig happens here }
  ParseString := ParseMetaData(MetaLogic, 0);
  { Afterwards, we do some replacements and Trimming to make the string more beautiful }
  ParseString := StringReplace(ParseString, '  ', ' ', [rfReplaceAll]);
  while pos(LineEnding + ' ',ParseString) > 0 do
    ParseString := StringReplace(ParseString, LineEnding + ' ', LineEnding, [rfReplaceAll]);
  ParseString := Trim(ParseString);
  Result := ParseString;
end;

function TSong.ParseMetaData(MetaLogic: string; count: integer): string;
var strArray: TStringArray;
  word, prop: String;
begin
  {Add a space to every lineending, so that the split is done successfully }
  strArray := MetaLogic.Split(' ');
  if count >= length(strArray) then exit(''); // End-Point of recursional function
  word := strArray[count];
  if (pos('{%', word) = 1) and (pos('%}', word) > 1) and (pos('end', word) <= 0) then
  begin
    prop := Trim(Copy(word, 3, pos('%}', word)-3));
    if self.MetaDict.IndexOf(lowerCase(prop)) >= 0 then
      Exit(StringReplace(word, '{%' + prop + '%}', '', []) + ' ' + ParseMetaData(MetaLogic, count+1))
    else begin
      while (pos('{%end%}',word)=0) and (pos(LineEnding,word)=0) and (count < length(strArray)-1) do
      begin
        inc(count);
        word := strArray[count];
      end;
      Exit(ParseMetaData(MetaLogic, count+1));
    end;
  end
  else if (pos('{', word) = 1) and (pos('}', word) > 1) then
  begin
    prop := Trim(Copy(word, 2, pos('}', word)-2));
    if self.MetaDict.IndexOf(lowerCase(prop)) >= 0 then
       word := StringReplace(word, '{' + prop + '}', self.MetaDict[lowerCase(prop)], [rfReplaceAll, rfIgnoreCase])
    else word := '';
  Result := word + ' ' + ParseMetaData(MetaLogic, count+1);
  end else if (pos('{%end%}',word)>0) then
    Result := StringReplace(word, '{%end%}', '', []) + ' ' + ParseMetaData(MetaLogic, count+1)
  else Result := word + ' ' + ParseMetaData(MetaLogic, count+1);
end;
end.

