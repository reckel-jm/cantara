unit lyrics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Strings, fgl;
type
  StringDict = specialize TFPGMap<string, string>;
  StringIntegerDict = specialize TFPGMap<string, integer>;


function StringListToString(StringList: TStringList): String;
function importCCLISongFile(filename: String): TStringList;
procedure ConvertFile(inputFile: TStringList; output: TStringList; MetaDict: StringDict);
procedure WritePart(inputFile: TStringList; output: TStringList; index: Integer);
procedure IncludeRepetitionalParts(inputFile: TStringList; output: TStringList; PositionDict: StringIntegerDict);

const
  CCLI_DELIM_COUNTER = 8;

implementation

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

function importCCLISongFile(filename: String): TStringList;
var inputFile, output: TStringList;
  MetaDict: StringDict; { Contains the MetaData like Title etc. }
begin
  inputFile := TStringList.Create;
  output := TStringList.Create;
  MetaDict := StringDict.Create;
  inputFile.LoadFromFile(filename);
  ConvertFile(inputFile, output, MetaDict);
  MetaDict.Free;
  InputFile.Free;
  Result := output;
end;

procedure ConvertFile(inputFile: TStringList; output: TStringList; MetaDict: StringDict);

var i: Integer;
  j: Integer;
  PositionDict: StringIntegerDict;
  RefrainState: Boolean;
begin
  PositionDict := StringIntegerDict.Create;
  MetaDict.Add('Title', inputFile.Strings[0]);
  RefrainState := False;
  for i :=  1 to inputFile.Count-1 do
  begin
    { The Parts Chorus and PreChorus are repeated after every other part (stanza+bridge). So, there position should be remembered. }
    if (pos('Chorus', inputFile.Strings[i]) = 1) or (inputFile.Strings[i] = 'PreChorus') then
    begin
      PositionDict.Add(inputFile.Strings[i],i); { Add the Element Name and the line number }
      WritePart(inputFile, Output, i);
      RefrainState := False;
    end else
    { The Parts Vers/Strophe and Bridge normally do not get repeated. They are only used once at the printed position. However, after them, the repetitional parts should follow. }
    if (pos('Strophe ',inputFile.Strings[i]) = 1) or (pos('Vers ',inputFile.Strings[i]) = 1) or (pos('Bridge',inputFile.Strings[i]) = 1) then
    begin
      if RefrainState = True then
         IncludeRepetitionalParts(inputFile, output, PositionDict);
      WritePart(inputFile, Output, i);
      RefrainState := True;
    end else
    { Handle the CCLI Copyright information }
    if (pos('CCLI', inputFile.Strings[i]) = 1) then
    begin
      if i < inputFile.Count-1 then
      begin
        MetaDict.Add('CCLI-Songnumber', inputFile.Strings[i].Split(' ')[1]);
        MetaDict.Add('Author',inputFile.Strings[i+1]);
      end else
      begin
        MetaDict.Add('CCLI-Licence', inputFile.Strings[i].Split(' ')[1]);
      end;
    end;
  end;
  { Add Closing Refrain if needed }
  if RefrainState = True then
    IncludeRepetitionalParts(inputFile, output, PositionDict);

  { Remove the last lines, if they are blank }
  While Output.Strings[Output.Count-1] = '' do
  begin
    Output.Delete(Output.Count-1);
  end;
  { Free the Position Dict }
  PositionDict.Free;
end;

procedure WritePart(inputFile: TStringList; output: TStringList; index: Integer);
var i,length: Integer;
begin
  i := index + 1;
  while (inputFile.Strings[i] <> '') and (i <= inputFile.Count-1) do
  begin
    output.Add(inputFile[i]); // Copy the Line
    inc(i);
  end;
  length := i - index - 1;
  if length >= CCLI_DELIM_COUNTER then { seperate the parts which are two big in two peaces }
  begin
    output.Insert(output.count-1-(length div 2)+1,'');
  end;
  output.Add(''); // An empty line at the end of a song part
end;

procedure IncludeRepetitionalParts(inputFile: TStringList; output: TStringList; PositionDict: StringIntegerDict);
var i, key: Integer;
  foundChorus: Boolean;
begin
  { Find the latest Chorus, print it as well }
  foundChorus := False;
  for i := 0 to PositionDict.Count-1 do
  begin
    if PositionDict.Keys[i] = 'PreChorus' then WritePart(inputFile, output, PositionDict.Data[i]);
    if pos('Chorus',PositionDict.Keys[i]) > 0 then
    begin
      key := PositionDict.Data[i]; foundChorus := True;
    end;
  end;
  if foundChorus then WritePart(inputFile, output, key);
end;

end.

