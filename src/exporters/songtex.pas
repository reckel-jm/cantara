{ <description>

  Copyright (C) <2022> <Jan Martin Reckel> <jm.reckel@t-online.de>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
unit SongTeX;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Lyrics;

type
  { TSongTexFile

  A TeX like file which exports a list of songs with their order and content.
  }
  TSongTeXFile = Class
  public
    NextSongFile: TStringList;
    SongTeXIsSelection: Boolean;
    constructor Create;
    destructor Destroy;
    procedure AddFile(SongFile: TRepoFile);
    procedure SaveTofile(FileName: String);
    procedure LoadFromFile(FileName: String);
    function HasNextSongfile: String;
    { Returns the content of the file as a string }
    function Text: String;
  private
    FileContent: TStringList;
    SongFiles: array of TRepoFile;
    ParsingIndex: Integer;
  end;

implementation

constructor TSongTeXFile.Create;
begin
  inherited;
  FileContent := TStringList.Create;
  FileContent.Add('% This file has been created automatically');
  FileContent.Add('% It can be opened with Cantara (https://cantara.app)');
  FileContent.Add('% Manually editing the content may damage the import');
  SongTeXIsSelection := True;
  ParsingIndex := -1;
  NextSongFile := TStringList.Create;
end;

destructor TSongTeXFile.Destroy;
begin
  FileContent.Free;
  NextSongFile.Destroy;
  inherited;
end;

procedure TSongTeXFile.AddFile(SongFile: TRepoFile);
var SongFileContent: TStringList;
begin
  SetLength(SongFiles, length(SongFiles)+1);
  SongFileContent := TStringList.Create;
  SongFileContent.LoadFromFile(SongFile.FilePath);
  FileContent.Add('\beginfile{' + SongFile.FileName + '}');
  fileContent.AddStrings(SongFileContent);
  FileContent.Add('\endfile');
end;

procedure TSongTeXFile.SaveTofile(FileName: String);
begin
  FileContent.SaveToFile(FileName);
end;

procedure TSongTeXFile.LoadFromFile(FileName: String);
begin
  FileContent.Clear;
  FileContent.LoadFromFile(FileName);
  ParsingIndex := 0;
  SongTeXIsSelection := not (
                     (Pos('\noselection', FileContent.Text) > 0)
                     );
end;

{
 This function returns the String of the Songname – if there is a next song – and adds it's content to
 the Stringlist NextSongFile.
 In case of no further songfiles available, an empty string '' will be returned.
}
function TSongTeXFile.HasNextSongfile: String;
var curSongname: string;
begin
  while (ParsingIndex <= FileContent.Count-1) and (pos('\beginfile{',FileContent.Strings[ParsingIndex]) < 1) do
  begin
    Inc(ParsingIndex);
  end;
  if (ParsingIndex <= FileContent.Count-1) and (pos('\beginfile{',FileContent.Strings[ParsingIndex]) > 0) then
  begin
    NextSongFile.Clear;
    curSongname := copy(FileContent.Strings[ParsingIndex],length('\beginfile{')+1,
                length(FileContent.Strings[ParsingIndex])-12);
    inc(ParsingIndex);
    while (ParsingIndex <= FileContent.Count-1) and (FileContent.Strings[ParsingIndex] <> '\endfile') do
    begin
      NextSongFile.Add(FileContent.Strings[ParsingIndex]);
      inc(ParsingIndex);
    end;
    Exit(curSongName);
  end;
  Result := '';
end;

function TSongTeXFile.Text: String;
begin
  Result := Self.FileContent.Text;
end;

end.

