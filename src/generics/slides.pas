{ Unit for handling the logic and structure of the slides in Cantara.
  The structure of the slide defines what a slide might represent, and which content is located in it.
  It does not handle the actual drawing.

  Copyright (C) 2023 Jan Martin Reckel

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
unit slides;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Lyrics, fgl, Dialogs, PresentationModels;

type

  { A Slide does have to have a slide type. Depending on the type, the content which is available and the drawing/export
  should is handled differently. }
  SlideTypeEnum = (
                { SlideType which represents a slide which shows a song title at the beginning of a song. }
                TitleSlide,
                { SlideType which represents a slide which shows song content with a spoiler (next part) }
                SlideWithSpoiler,
                { SlideType which represents a slide which shows song content without a spoiler (next part) }
                SlideWithoutSpoiler,
                { SlideType which represents a slide which is empty and used as a placeholder }
                EmptySlide
                );

  {
    This record contains settings required for the correct generation of slides and its contents out of a song.
    It will be most likely loaded from the user settings.
  }
  TSlideSettings = record
    { Defines whether spoiler text is used. }
    SpoilerText: Boolean;
    { Defines whether there should be a title slide generated }
    TitleSlide: Boolean;
    { Show meta information in the buttom left corner of the first slide with song content }
    FirstSlideMeta: Boolean;
    { Show meta information in the buttom left corner of the last slide with song content }
    LastSlideMeta: Boolean;
    { Syntax used for generating meta information }
    MetaSyntax: String;
    { Add an empty slide between songs }
    EmptySlideBetweenSongs: Boolean;
    { The maximal number of lines which are allowed at one line }
    MaxSlideLineLength: Integer;
  end;

  { An Element of every slide which represents its text content. }
  TPartContent = class
  public
    MainText: String;
    SpoilerText: String;
    MetaText: String;
    constructor Create; overload;
  end;

  { Represents an abstraction of a slide to be shown. }
  TSlide = class
  public
    Song: TSong;
    PartContent: TPartContent;
    SlideType: SlideTypeEnum;
    ID: Integer;
    { When True, CustomStyle overrides the global presentation style for this slide. }
    HasCustomStyle: Boolean;
    CustomStyle: TPresentationStyleSettings;
    constructor Create; overload;
    destructor Destroy; override;

  end;

  TSlideList = specialize TFPGObjectList<TSlide>;

{ Create Presentation Data from a TSong with appropriate settings }
function CreatePresentationDataFromSong(Song: TSong; SlideSettings: TSlideSettings;
  var SlideCounter: Integer): TSlideList;
{ Splits the song source slides }
function SplitSlides(Input: TStringList; MaxSlides: Integer): String;

implementation

constructor TPartContent.Create;
begin
  inherited;
  MainText := '';
  SpoilerText := '';
  MetaText := '';
end;

constructor TSlide.Create;
begin
  inherited;
  self.PartContent := TPartContent.Create;
  self.HasCustomStyle := False;
end;

destructor TSlide.Destroy;
begin
  FreeAndNil(self.PartContent);
  if HasCustomStyle then
    DestroyPresentationStyleSettings(CustomStyle);
  inherited;
end;

function IsBilingual(var MainPart: String): String;
var
  LanguageSeperatorPosition: Integer;
  SecondLanguagePart: String;
begin
  LanguageSeperatorPosition := Pos('---', MainPart);
  if (LanguageSeperatorPosition > 0) And (Length(MainPart) >
    LanguageSeperatorPosition + 3) then
  begin
    SecondLanguagePart := Copy(MainPart, LanguageSeperatorPosition +
      3, Length(MainPart) - LanguageSeperatorPosition - 2 + 1);
    MainPart := Trim(Copy(MainPart, 1, LanguageSeperatorPosition - 1));
  end
  else
    SecondLanguagePart := '';
  Result := Trim(SecondLanguagePart);
end;

function CreatePresentationDataFromSong(Song: TSong; SlideSettings: TSlideSettings;
  var SlideCounter: Integer): TSlideList;
var
  songfile: TStringList;
  completefilename: String;
  songname: String;
  stanza: String;
  slidestring: String;
  slides: array of String;
  InputSlides: TStringList;
  CurrentSongSlideList: TSlideList;
  Slide: TSlide;
  i, j: Integer;
  SecondLanguageText: String;
begin
  { Create the SlideList which later will be returned }
  CurrentSongSlideList := TSlideList.Create(False);
  SongFile := TStringList.Create;
  { Split the slides if desired }
  if SlideSettings.MaxSlideLineLength > 0 then
    songfile.Text := SplitSlides(Song.output, SlideSettings.MaxSlideLineLength)
  else
    songfile.Assign(song.output);
  //gehe durch Songdatei und füge gleiche Strophen zu einem String zusammen
  stanza := '';
  for j := 0 to songfile.Count - 1 do
  begin
    if (songfile.strings[j] = '') then
    begin
      Slide := TSlide.Create;
      Slide.Song := Song;
      Slide.PartContent.MainText := Trim(stanza);
      Slide.SlideType := SlideWithoutSpoiler;
      Slide.ID := SlideCounter;
      SlideCounter += 1;
      stanza := '';
      { Find out whether song is billingual }
      SecondLanguageText := IsBilingual(Slide.PartContent.MainText);
      if SecondLanguageText <> '' then
      begin
        Slide.PartContent.SpoilerText := SecondLanguageText;
        Slide.SlideType := SlideWithSpoiler;
      end;
      CurrentSongSlideList.Add(Slide);
    end
    else
      stanza := stanza + songfile.Strings[j] + LineEnding;
  end;
  { Add the last stanza }
  Slide := TSlide.Create;
  Slide.Song := Song;
  Slide.PartContent.MainText := Trim(stanza);
  Slide.ID := SlideCounter;
  Slide.SlideType := SlideWithoutSpoiler;
  Inc(SlideCounter);

  { Find out whether song is billingual }
  SecondLanguageText := IsBilingual(Slide.PartContent.MainText);
  if SecondLanguageText <> '' then
  begin
    Slide.PartContent.SpoilerText := SecondLanguageText;
    Slide.SlideType := SlideWithSpoiler;
  end;

  CurrentSongSlideList.Add(Slide);

  { Add Spoiler Text to the slides if desired in the settings }

  if SlideSettings.SpoilerText then
  begin
    for j := 0 to CurrentSongSlideList.Count - 2 do
      if CurrentSongSlideList.Items[j].PartContent.SpoilerText = '' then
      begin
        CurrentSongSlideList.Items[j].PartContent.SpoilerText :=
          Trim(CurrentSongSlideList.Items[j + 1].PartContent.MainText);
        CurrentSongSlideList.Items[j].SlideType := SlideWithSpoiler;
      end;
  end;

  { Add Meta Information to the slides if desired in the settings which were handed over }

  if (SlideSettings.FirstSlideMeta) then
  begin
    CurrentSongSlideList.Items[0].PartContent.MetaText :=
      Song.ParseMetaData(SlideSettings.MetaSyntax);
  end;

  if (SlideSettings.LastSlideMeta) then
  begin
    CurrentSongSlideList.Items[CurrentSongSlideList.Count - 1].PartContent.MetaText :=
      Song.ParseMetaData(SlideSettings.MetaSyntax);
  end;

  { Add an empty frame if selected in the settings }
  if SlideSettings.EmptySlideBetweenSongs then
  begin
    // We create a slide but with no content
    Slide := TSlide.Create;
    Slide.Song := Song;
    Slide.ID := SlideCounter;
    Slide.SlideType := EmptySlide;
    Inc(SlideCounter);
    CurrentSongSlideList.Add(Slide);
  end;

  { add title slide if desired in the settings }
  // We are doing that at the last slide because we want to avoid confusion with firstslide/lastslide BEFOREHAND

  if SlideSettings.TitleSlide then
  begin
    Slide := TSlide.Create;
    Slide.Song := Song;
    Slide.PartContent.MainText := Trim(Song.MetaDict['title']);
    Slide.PartContent.SpoilerText := Trim(Song.ParseMetaData(SlideSettings.MetaSyntax));
    Slide.SlideType := TitleSlide;
    CurrentSongSlideList.Insert(0, Slide);
  end;
  { Clean Up }
  SongFile.Destroy;
  { Return CurrentSongSlideList }
  Result := CurrentSongSlideList;
end;

function SplitSlides(Input: TStringList; MaxSlides: Integer): String;
var
  n1, n2, i, j, t, place: Integer;
  changed: Boolean;
  output: TStringList;
  LastNormalStanza: Integer;
  SecondLanguageStringList: TStringList;
  LastDelim: String;
begin
  output := TStringList.Create;
  output.Assign(input);
  if output.Count = 0 then // If we have an empty document
  begin
    Result := '';
    Exit;
  end;
  if MaxSlides <= 0 then exit;
  // Just as a protective measure, actually not needed anymore.
  if MaxSlides = 1 then
    // it means to have one line per slide, so we take a shortpath
  begin
    SecondLanguageStringList := TStringList.Create;
    i := 0;
    LastDelim := '';
    LastNormalStanza := 0;
    while i < output.Count - 1 do
    begin
      if (output.Strings[i] = '') Or (output.Strings[i] = '---') then
      begin
        LastDelim := output.Strings[i];
        if (output.Strings[i] = '') And (i < output.Count - 1) then
          LastNormalStanza := i + 1
        else
        if output.Strings[i] = '---' then
        begin
          if i < output.Count - 1 then
          begin
            output.Delete(i);
            j := i;
            SecondLanguageStringList.Clear;
            while (j < output.Count) And (output.Strings[j] <> '') do
            begin
              SecondLanguageStringList.Add(output.Strings[j]);
              output.Delete(j);
            end;
            i := LastNormalStanza;
            for t := 0 to SecondLanguageStringlist.Count - 1 do
            begin
              if i < output.Count - 1 then output.Insert(i + 1, '---')
              else
                output.Add('---');
              if i < output.Count - 2 then
                output.Insert(i + 2, SecondLanguageStringlist.Strings[t])
              else
                output.Add(SecondLanguageStringlist.Strings[t]);
              if i < output.Count - 3 then output.Insert(i + 3, '')
              else
                output.Add('');
              i := i + 5;
            end;
            while (i < output.Count) And (output.Strings[i] <> '') do i += 1;
            if i < output.Count - 1 then LastNormalStanza := i + 1;
          end;
        end;
      end
      else
      if (output.Strings[i] <> '') And (output.Strings[i] <> '---') then
      begin
        output.Insert(i + 1, '');
        i := i + 1;
      end;
      i := i + 1;
    end;
    SecondLanguageStringList.Destroy;
  end
  else
  begin
    repeat
      begin
        changed := False;
        n1 := 0;
        n2 := 0;
        output.Add('');
        for i := 0 to output.Count - 1 do
        begin
          if (output.Strings[i] = '') Or (output.Strings[i] = '---') then
          begin
            n2 := i;
            if (n2 - n1) > MaxSlides then
            begin
              if MaxSlides Mod 2 = 0 then
                place := (n1 + (n2 - n1) Div 2)
              else
                place := (n1 + (n2 - n1) Div 2) + 1;
              output.Insert(place, '');
              if (output.Strings[i + 1] = '---') then
              begin
                if output.Count > n2 + 1 + place - n1 then
                begin
                  //output.Strings[place] := '---';
                  for t := 0 to place - n1 do
                  begin
                    output.Move(n2 + 1 + t, place + t);
                  end;
                  output.Strings[n2 + 1] := '';
                  output.insert(n2 + 1 + place - n1 + 1, '---');
                end;
              end;
              changed := True;
            end;
            n1 := n2 + 1;
          end;
        end;
      end
    until changed = False;
  end;
  while (output.Strings[output.Count - 1] = '') do output.Delete(output.Count - 1);
  output.Text := StringReplace(output.Text, LineEnding + LineEnding +
    LineEnding, LineEnding + LineEnding, [rfReplaceAll]);
  Result := output.Text;
  Output.Destroy;
end;

end.
