unit lyrics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Strings, fgl, Dialogs, LazFileUtils;

function StringListToString(StringList: TStringList): String;

type
  TStringDict = specialize TFPGMap<string, string>;
  TStringIntegerDict = specialize TFPGMap<string, integer>;
  TRepoFile = class
    public
      Name: string;
      FileName: string;
      FilePath: String;
      FileExtension: String;
  end;
  TRepoArray = array of TRepoFile;

  { TSong }
  { The class TSong represents a song in Cantara and implements the logic for importing and parsing.
  At the moment, two formats are supported:
   * CCLI-Songselect
   * Song Format }
  TSong = Class
    public
      { The file path where the song is located physically on the filesystem }
      CompleteFilePath: String;
      { The file path without ending }
      FileNameWithoutEnding: String;
      { contains the lyrics in their right order }
      output: TStringList;
      { contains all the MetaData of the songs }
      MetaDict: TStringDict;
      MaxSlideLineLength: Integer; { When should slices be cut into two parts }
      constructor Create; overload;
      destructor Destroy; override;
      procedure importSongFile;
      procedure importSongfile(filepath: string);
      {
      This is the main function which converts a CCLI file into the linear song format.
      It is public – that means can be called outside – but will call private class functions when needed.
      }
      procedure ConvertCCLIFile;
      procedure slideWrap;
      function ParseMetaData(MetaLogic: string): string;
      procedure exportAsSongFile(outputfilename: String);
      function IsCCLIFile: Boolean;
      procedure strip;
      procedure importSongFromStringList(stringlist: TStringList);
      { Resets the song to its original state }
      procedure Reset;
    private
      inputFile: TStringList;
      PositionDict: TStringIntegerDict;
      procedure WritePart(index: Integer);
      { For CCLI-to-Song-Convertion: Finds and inserts the appropriate repetitional parts (mostly used after stanza):
      1. Pre-Chorus(es)
      2. The latest Chorus }
      procedure IncludeRepetitionalParts;
      { For CCLI-to-Song-Convertion: Finds and inserts the latest Refrain. }
      procedure IncludeLatestRefrain;
      procedure importSongFormatFile;
      procedure importCCLISongFile;
      procedure importCCLISongFile(filepath: string);
      procedure getSongNameWithoutEnding;
      function ParseMetaData(MetaLogic: string; count: integer): string;
      function compareWithOtherSong(TheSong: TSong): Boolean;
      procedure DecideFileFormatAndContinue;
  end;
  TSongList = specialize TFPGObjectList<TSong>;


operator < (var A, B: TSong): Boolean;
operator > (var A, B: TSong): Boolean;
//operator = (const A, B: TSong): Boolean;

const
  CCLIWEBPAGE:string = 'www.ccli.com';

implementation

operator < (var A, B: TSong): Boolean;
begin
  Result := A.CompleteFilePath < B.CompleteFilePath;
end;

operator > (var A, B: TSong): Boolean;
begin
  Result := A.CompleteFilePath > B.CompleteFilePath;
end;

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

procedure TSong.getSongNameWithoutEnding;
begin
  if ExtractFileExt(self.CompleteFilePath) <> '' then
    self.FileNameWithoutEnding := ExtractFilename(copy(self.CompleteFilePath,1,pos(ExtractFileExt(self.CompleteFilePath),self.CompleteFilePath)-1))
  else
    self.FileNameWithoutEnding := ExtractFilename(self.CompleteFilePath);
end;

procedure TSong.importCCLISongFile;
begin
  GetSongNameWithoutEnding;
  ConvertCCLIFile;
end;

procedure TSong.importCCLISongFile(filepath: String);
begin
  self.CompleteFilePath := filepath;
  self.importCCLISongFile;
end;

{
This is the main function which converts a CCLI file into the linear song format.
It is public – that means can be called outside – but will call private class functions when needed.
}
procedure TSong.ConvertCCLIFile;
var i: Integer;
  j: Integer;
  RefrainState: Boolean;
begin
  self.MetaDict.Add('title', inputFile.Strings[0]);
  RefrainState := False;
  for i :=  1 to self.inputFile.Count-1 do
  begin
    { The Parts Chorus and PreChorus are repeated after every other part (stanza+bridge). So, there position should be remembered. }
    if (pos('Chorus', self.inputFile.Strings[i]) = 1) or (self.inputFile.Strings[i] = 'PreChorus') or (self.inputFile.Strings[i] = 'Pre-Chorus') then
    begin
      PositionDict.Add(self.inputFile.Strings[i],i); { Add the Element Name and the line number }
      WritePart(i);
      RefrainState := False;
    end else
    { The Parts Vers/Strophe and Bridge normally do not get repeated. They are only used once at the printed position. However, after them, the repetitional parts should follow. }
    if (pos('Strophe ',inputFile.Strings[i]) = 1) or (pos('Vers ',inputFile.Strings[i]) = 1) or (inputFile.Strings[i] = 'Vers') then
    begin
      if RefrainState = True then
         IncludeRepetitionalParts;
      WritePart(i);
      RefrainState := True;
    end else
    // We assume that after a bridge no Pre-Chorus parts should be included, just the (latest used) chorus.
    if (pos('Bridge',inputFile.Strings[i]) = 1) then
    begin
      if RefrainState = True then
         IncludeLatestRefrain;
      WritePart(i);
      RefrainState := True;
    end else
    { Handle the CCLI Copyright information }
    if (pos('CCLI', inputFile.Strings[i]) = 1) then
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
  self.output.Add(''); // An empty line at the end of a song part
end;

{
 For CCLI-to-Song-Convertion: Finds and inserts the appropriate repetitional parts (mostly used after stanza):
 1. Pre-Chorus(es)
 2. The latest Chorus
}
procedure TSong.IncludeRepetitionalParts;
var i, key: Integer;
  foundChorus: Boolean;
begin
  { Find the latest Chorus, print it as well }
  foundChorus := False;
  for i := 0 to self.PositionDict.Count-1 do
  begin
    if (LowerCase(self.PositionDict.Keys[i]) = LowerCase('PreChorus')) or (LowerCase(self.PositionDict.Keys[i]) = LowerCase('Pre-Chorus')) then WritePart(self.PositionDict.Data[i]);
    if pos('Chorus',self.PositionDict.Keys[i]) > 0 then
    begin
      key := self.PositionDict.Data[i]; foundChorus := True;
    end;
  end;
  if foundChorus then WritePart(key);
end;

{
 For CCLI-to-Song-Convertion: Finds and inserts the latest Refrain.
}
procedure TSong.IncludeLatestRefrain;
var i, key: Integer;
  foundChorus: Boolean;
begin
  foundChorus := False;
  for i := 0 to self.PositionDict.Count-1 do
  begin
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
  GetSongNameWithoutEnding;
  // We add the title of the song from the CompleteFilePath (will be overridden if stated otherwise)
  self.MetaDict.Add('title', self.FileNameWithoutEnding);
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
  if self.MaxSlideLineLength <= 0 then exit; // Just as a protective measure, actually not needed anymore.
  if self.MaxSlideLineLength = 1 then       // it means to have one line per slide, so we take a shortpath
  begin
    i := 0;
    while i < output.count-1 do
      begin
        if output.Strings[i] <> '' then
          output.Insert(i+1, '');
        i := i+1;
      end;
    Break;
  end;
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
  output.Text:=StringReplace(output.Text, LineEnding+LineEnding+LineEnding, LineEnding+LineEnding, [rfReplaceAll]);
end;

procedure TSong.DecideFileFormatAndContinue;
var songfileextension: String;
begin
  songfileextension := ExtractFileExt(self.CompleteFilePath);
  if songfileextension = '.song' then
    self.importSongFormatFile
  else if self.IsCCLIFile then // CCLI-Songselect file
    self.importCCLISongFile;
  if self.MaxSlideLineLength>0 then self.slideWrap;
end;

{ This function finds out which format the song has and calls the specific import function }
procedure TSong.importSongFile;
begin
  if self.CompleteFilePath <> '' then
     self.inputFile.LoadFromFile(self.CompleteFilePath);
  DecideFileFormatAndContinue;
  self.strip;
end;

procedure TSong.importSongFile(filepath: String);
begin
  self.CompleteFilePath:=filepath;
  importSongFile;
end;

procedure TSong.importSongFromStringList(stringlist: TStringList);
begin
  self.inputFile.Assign(stringlist);
  if self.IsCCLIFile then // CCLI-Songselect file
    self.importCCLISongFile
  else self.importSongFormatFile;
  if self.MaxSlideLineLength>0 then self.slideWrap;
  self.strip;
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

procedure TSong.exportAsSongFile(outputfilename: String);
var outputcontent: TStringList;
  i: integer;
begin
  outputcontent := TStringList.Create;
  for i := 0 to self.MetaDict.Count-1 do
    outputcontent.Add('#' + self.MetaDict.Keys[i] + ': ' + self.MetaDict.Data[i]);
  outputcontent.Add('');
  outputcontent.AddStrings(self.output);
  outputcontent.SaveToFile(outputfilename);
  FreeAndNil(outputcontent);
end;

function TSong.IsCCLIFile: Boolean;
var i: integer;
begin
  if ExtractFileExt(self.CompleteFilePath) = '.song' then exit(False);
  if ExtractFileExt(self.CompleteFilePath) = '.ccli' then exit(True)
  else if ExtractFileExt(self.CompleteFilePath) = '.txt' then
  begin
  for i := 0 to inputFile.Count-1 do
      if (pos(CCLIWEBPAGE, inputfile.Strings[i]) > 0) or (pos('CCLI', inputfile.Strings[i]) > 0) then exit(True);
  end;
  Result := False;
end;

procedure TSong.strip;
var i: integer;
begin
  i := output.Count-1;
  try
  while (i > 0) and (Trim(output.Strings[i]) = '') do  // We leave at least one line
  begin
    output.Delete(i);
    i := i - 1;
  end;
  finally
  end;
end;

function TSong.compareWithOtherSong(TheSong: TSong): Boolean;
var i: integer;
begin
  self.strip;
  TheSong.strip;
  if self.output.Count <> TheSong.output.Count then Exit(False);
  for i := 0 to self.output.Count-1 do
    if self.output.Strings[i] <> TheSong.output.Strings[i] then Exit(False);
  if self.MetaDict.Count <> TheSong.MetaDict.Count then Exit(False);
  for i := 0 to self.MetaDict.Count-1 do
    if ((self.MetaDict.Keys[i] <> TheSong.MetaDict.Keys[i]) and (self.MetaDict.Data[i] <> TheSong.MetaDict.Data[i]))
       then exit(False);
  // If the function arrives here, the two songs should be the same.
  Result := True;
end;

procedure TSong.Reset;
begin
  if (self.inputFile.Text = '') or (self.output.Text = '') then exit;
  self.output.Clear;
  self.MaxSlideLineLength:=0;
  self.FileNameWithoutEnding:='';
  self.importSongFile;
end;

end.

