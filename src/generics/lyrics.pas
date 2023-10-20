{ <description>

  Copyright (C) 2023 Jan Martin Reckel <jm.reckel@t-online.de>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.

}

{
This Unit contains all functions for handling a song file which contains only
lyrics. It implements parsing of several input file formats (CCLI/Song file format)
and provides structures for handling song parts with text.

@author(Jan Martin Reckel)
}
unit lyrics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Strings, fgl, Dialogs;

function StringListToString(StringList: TStringList): String;

type
  TStringDict = specialize TFPGMap<String, String>;
  TStringIntegerDict = specialize TFPGMap<String, Integer>;

  TRepoFile = class
  public
    Name: String;
    FileName: String;
    FilePath: String;
    FileExtension: String;
  end;

  TRepoArray = array of TRepoFile;

  { TSong }
  { The class TSong represents a song in Cantara and implements the logic for importing and parsing.
  At the moment, two formats are supported:
   * CCLI-Songselect
   * Song Format }
  TSong = class
  public
    { The file path where the song is located physically on the filesystem }
    CompleteFilePath: String;
    { The file path without ending }
    FileNameWithoutEnding: String;
    { contains the lyrics in their right order }
    output: TStringList;
    { contains all the MetaData of the songs }
    MetaDict: TStringDict;
    constructor Create; overload;
    destructor Destroy; override;
    { Function to import a song file into the song }
    procedure importSongfile(filepath: String);
    function ParseMetaData(MetaLogic: String): String;
    procedure exportAsSongFile(outputfilename: String);
    function ExportAsSongFile: String;
    function IsCCLIFile: Boolean;
    procedure strip;
    procedure importSongFromStringList(stringlist: TStringList);
    { Resets the song to its original state }
    procedure Reset;
    function IsEmpty: Boolean;
  private
    inputFile: TStringList;
    PositionDict: TStringIntegerDict;
    procedure importSongFile;
      {
      This is the main function which converts a CCLI file into the linear song format.
      It is not public to prevent exceptions if the file is not a CCLI file.
      It will be called by importSongFile if necessary.
      }
    procedure ConvertCCLIFile;
    procedure WritePart(index: Integer);
      { For CCLI-to-Song-Convertion: Finds and inserts the appropriate repetitional parts (mostly used after stanza):
      1. Pre-Chorus(es)
      2. The latest Chorus }
    procedure IncludeRepetitionalParts;
    { For CCLI-to-Song-Convertion: Finds and inserts the latest Refrain. }
    procedure IncludeLatestRefrain;
    procedure importSongFormatFile;
    procedure importCCLISongFile;
    procedure importCCLISongFile(filepath: String);
    procedure getSongNameWithoutEnding;
    function ParseMetaData(MetaLogic: String; Count: Integer): String;
    function compareWithOtherSong(TheSong: TSong): Boolean;
    procedure DecideFileFormatAndContinue;
  end;

  TSongList = specialize TFPGObjectList<TSong>;


operator < (var A, B: TSong): Boolean;
operator > (var A, B: TSong): Boolean;
//operator = (const A, B: TSong): Boolean;

const
  CCLIWEBPAGE: String = 'www.ccli.com';

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
    self.FileNameWithoutEnding :=
      ExtractFilename(copy(self.CompleteFilePath, 1, pos(
      ExtractFileExt(self.CompleteFilePath), self.CompleteFilePath) - 1))
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
var
  i: Integer;
  j: Integer;
  RefrainState: Boolean;
begin
  self.MetaDict.Add('title', inputFile.Strings[0]);
  RefrainState := False;
  for i := 1 to self.inputFile.Count - 1 do
  begin
    { The Parts Chorus and PreChorus are repeated after every other part (stanza+bridge). So, there position should be remembered. }
    if (pos('Chorus', self.inputFile.Strings[i]) = 1) Or
      (self.inputFile.Strings[i] = 'PreChorus') Or
      (self.inputFile.Strings[i] = 'Pre-Chorus') then
    begin
      PositionDict.Add(self.inputFile.Strings[i], i);
      { Add the Element Name and the line number }
      WritePart(i);
      RefrainState := False;
    end
    else
    { The Parts Vers/Strophe and Bridge normally do not get repeated. They are only used once at the printed position. However, after them, the repetitional parts should follow. }
    if (pos('Strophe ', inputFile.Strings[i]) = 1) Or
      (pos('Vers ', inputFile.Strings[i]) = 1) Or (inputFile.Strings[i] = 'Vers') then
    begin
      if RefrainState = True then
        IncludeRepetitionalParts;
      WritePart(i);
      RefrainState := True;
    end
    else
    // We assume that after a bridge no Pre-Chorus parts should be included, just the (latest used) chorus.
    if (pos('Bridge', inputFile.Strings[i]) = 1) then
    begin
      if RefrainState = True then
        IncludeLatestRefrain;
      WritePart(i);
      RefrainState := True;
    end
    else
    { Handle the CCLI Copyright information }
    if (pos('CCLI', inputFile.Strings[i]) = 1) then
    begin
      if i < self.inputFile.Count - 1 then
      begin
        self.MetaDict.Add('ccli-songnumber', self.inputFile.Strings[i].Split(' ')[1]);
        self.MetaDict.Add('author', self.inputFile.Strings[i + 1]);
      end
      else
      begin
        self.MetaDict.Add('ccli-licensenumber', self.inputFile.Strings[i].Split(' ')[1]);
      end;
    end;
  end;
  { Add Closing Refrain if needed }
  if RefrainState = True then
    IncludeRepetitionalParts;

  { Remove the last lines, if they are blank }
  while self.Output.Strings[self.Output.Count - 1] = '' do
  begin
    self.Output.Delete(self.Output.Count - 1);
  end;
end;

procedure TSong.WritePart(index: Integer);
var
  i, length: Integer;
begin
  i := index + 1;
  while (self.inputFile.Strings[i] <> '') And (i <= self.inputFile.Count - 1) do
  begin
    self.output.Add(self.inputFile[i]); // Copy the Line
    Inc(i);
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
var
  i, key: Integer;
  foundChorus: Boolean;
begin
  { Find the latest Chorus, print it as well }
  foundChorus := False;
  for i := 0 to self.PositionDict.Count - 1 do
  begin
    if (LowerCase(self.PositionDict.Keys[i]) = LowerCase('PreChorus')) Or
      (LowerCase(self.PositionDict.Keys[i]) = LowerCase('Pre-Chorus')) then
      WritePart(self.PositionDict.Data[i]);
    if pos('Chorus', self.PositionDict.Keys[i]) > 0 then
    begin
      key := self.PositionDict.Data[i];
      foundChorus := True;
    end;
  end;
  if foundChorus then WritePart(key);
end;

{
 For CCLI-to-Song-Convertion: Finds and inserts the latest Refrain.
}
procedure TSong.IncludeLatestRefrain;
var
  i, key: Integer;
  foundChorus: Boolean;
begin
  foundChorus := False;
  for i := 0 to self.PositionDict.Count - 1 do
  begin
    if pos('Chorus', self.PositionDict.Keys[i]) > 0 then
    begin
      key := self.PositionDict.Data[i];
      foundChorus := True;
    end;
  end;
  if foundChorus then WritePart(key);
end;

function StringListToString(StringList: TStringList): String;
var
  s: String;
  i: Integer;
begin
  s := '';
  if StringList.Count < 1 then exit(s);
  for i := 0 to StringList.Count - 2 do
  begin
    s := s + StringList.Strings[i] + LineEnding;
  end;
  Result := s;
end;

procedure TSong.importSongFormatFile;
var
  i: Integer;
  curLineText, key, Value: String;
  contentStarted: Boolean;
begin
  GetSongNameWithoutEnding;
  // We add the title of the song from the CompleteFilePath (will be overridden if stated otherwise)
  self.MetaDict.Add('title', self.FileNameWithoutEnding);
  contentStarted := False;
  for i := 0 to self.inputFile.Count - 1 do
  begin
    curLineText := self.inputFile.Strings[i];
    if pos('#', curLineText) = 1 then
    begin
      if pos(':', curLineText) > 1 then
        key := curLineText.Split(':')[0];
      Delete(key, 1, 1); // Delete the # at the beginning
      Value := curLineText.Split(':')[1];
      { Remove Whitespaces
        If none of the parts are empty, add the key-value-pair to the MetaData dictionary }
      if (key <> '') And (Value <> '') then
      begin
        key := trim(key);
        Value := trim(Value);
      end;
      self.MetaDict.AddOrSetData(lowerCase(key), Value);
    end
    else
    if (Trim(curLineText) = '') And (contentStarted = True) then output.Add(curLineText)
    else if (Trim(curLineText) <> '') then
    begin
      output.Add(curLineText);
      contentStarted := True;
    end;
  end;
end;

procedure TSong.DecideFileFormatAndContinue;
var
  songfileextension: String;
begin
  songfileextension := ExtractFileExt(self.CompleteFilePath);
  if songfileextension = '.song' then
    self.importSongFormatFile
  else if self.IsCCLIFile then // CCLI-Songselect file
    self.importCCLISongFile;
end;

{ This function finds out which format the song has and calls the specific import function }
procedure TSong.importSongFile;
begin
  if self.CompleteFilePath <> '' then
    self.inputFile.LoadFromFile(self.CompleteFilePath);
  DecideFileFormatAndContinue;
  self.strip;
end;

procedure TSong.importSongfile(filepath: String);
begin
  self.CompleteFilePath := filepath;
  importSongFile;
end;

procedure TSong.importSongFromStringList(stringlist: TStringList);
begin
  self.inputFile.Assign(stringlist);
  self.importSongFormatFile;
end;

function TSong.ParseMetaData(MetaLogic: String): String;
var
  ParseString: String;
begin
  { Prepare String for Syntax }
  MetaLogic := StringReplace(MetaLogic, LineEnding, ' ' + LineEnding +
    ' ', [rfReplaceAll, rfIgnoreCase]);
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
  while pos(LineEnding + ' ', ParseString) > 0 do
    ParseString := StringReplace(ParseString, LineEnding + ' ', LineEnding,
      [rfReplaceAll]);
  ParseString := Trim(ParseString);
  Result := ParseString;
end;

function TSong.ParseMetaData(MetaLogic: String; Count: Integer): String;
var
  strArray: TStringArray;
  Word, prop: String;
begin
  {Add a space to every lineending, so that the split is done successfully }
  strArray := MetaLogic.Split(' ');
  if Count >= length(strArray) then exit(''); // End-Point of recursional function
  Word := strArray[Count];
  if (pos('{%', Word) = 1) And (pos('%}', Word) > 1) And (pos('end', Word) <= 0) then
  begin
    prop := Trim(Copy(Word, 3, pos('%}', Word) - 3));
    if self.MetaDict.IndexOf(lowerCase(prop)) >= 0 then
      Exit(StringReplace(Word, '{%' + prop + '%}', '', []) + ' ' +
        ParseMetaData(MetaLogic, Count + 1))
    else
    begin
      while (pos('{%end%}', Word) = 0) And (pos(LineEnding, Word) = 0) And
        (Count < length(strArray) - 1) do
      begin
        Inc(Count);
        Word := strArray[Count];
      end;
      Exit(ParseMetaData(MetaLogic, Count + 1));
    end;
  end
  else if (pos('{', Word) = 1) And (pos('}', Word) > 1) then
  begin
    prop := Trim(Copy(Word, 2, pos('}', Word) - 2));
    if self.MetaDict.IndexOf(lowerCase(prop)) >= 0 then
      Word := StringReplace(Word, '{' + prop + '}', self.MetaDict[lowerCase(prop)],
        [rfReplaceAll, rfIgnoreCase])
    else
      Word := '';
    Result := Word + ' ' + ParseMetaData(MetaLogic, Count + 1);
  end
  else if (pos('{%end%}', Word) > 0) then
    Result := StringReplace(Word, '{%end%}', '', []) + ' ' +
      ParseMetaData(MetaLogic, Count + 1)
  else
    Result := Word + ' ' + ParseMetaData(MetaLogic, Count + 1);
end;

procedure TSong.exportAsSongFile(outputfilename: String);
var
  outputcontent: TStringList;
  i: Integer;
begin
  try
    outputcontent := TStringList.Create;
    outputcontent.Text := self.ExportAsSongFile;
    outputcontent.SaveToFile(outputfilename);
  finally
    outputcontent.Destroy;
  end;
end;

function TSong.ExportAsSongFile: String;
var
  outputcontent: TStringList;
  i: Integer;
begin
  outputcontent := TStringList.Create;
  for i := 0 to self.MetaDict.Count - 1 do
    outputcontent.Add('#' + self.MetaDict.Keys[i] + ': ' + self.MetaDict.Data[i]);
  outputcontent.Add('');
  outputcontent.AddStrings(self.output);
  Result := Trim(outputcontent.Text);
  OutputContent.Destroy;
end;

function TSong.IsCCLIFile: Boolean;
var
  i: Integer;
begin
  if ExtractFileExt(self.CompleteFilePath) = '.song' then exit(False);
  if ExtractFileExt(self.CompleteFilePath) = '.ccli' then exit(True)
  else if ExtractFileExt(self.CompleteFilePath) = '.txt' then
  begin
    for i := 0 to inputFile.Count - 1 do
      if (pos(CCLIWEBPAGE, inputfile.Strings[i]) > 0) Or
        (pos('CCLI', inputfile.Strings[i]) > 0) then exit(True);
  end;
  Result := False;
end;

procedure TSong.strip;
var
  i: Integer;
begin
  i := output.Count - 1;
  try
    while (i > 0) And (Trim(output.Strings[i]) = '') do  // We leave at least one line
    begin
      output.Delete(i);
      i := i - 1;
    end;
  finally
  end;
end;

function TSong.compareWithOtherSong(TheSong: TSong): Boolean;
var
  i: Integer;
begin
  self.strip;
  TheSong.strip;
  if self.output.Count <> TheSong.output.Count then Exit(False);
  for i := 0 to self.output.Count - 1 do
    if self.output.Strings[i] <> TheSong.output.Strings[i] then Exit(False);
  if self.MetaDict.Count <> TheSong.MetaDict.Count then Exit(False);
  for i := 0 to self.MetaDict.Count - 1 do
    if ((self.MetaDict.Keys[i] <> TheSong.MetaDict.Keys[i]) And
      (self.MetaDict.Data[i] <> TheSong.MetaDict.Data[i])) then exit(False);
  // If the function arrives here, the two songs should be the same.
  Result := True;
end;

procedure TSong.Reset;
begin
  if (self.inputFile.Text = '') Or (self.output.Text = '') then exit;
  self.output.Clear;
  self.FileNameWithoutEnding := '';
  self.importSongFile;
end;

function TSong.IsEmpty: Boolean;
begin
  Result := (output.Count = 0);
end;

end.
