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
  OutputList: TStringList;
  Lang1Lines, Lang2Lines: TStringList;
  i: Integer;
  IsSecondLang: Boolean;
  Line: String;

  procedure FlushVerse;
  var sIdx, lIdx, vSlides: Integer;
  begin
    if (Lang1Lines.Count = 0) and (Lang2Lines.Count = 0) then Exit;

    // Determining number of slides only from first language
    vSlides := (Lang1Lines.Count + MaxSlides - 1) div MaxSlides;
    if vSlides = 0 then vSlides := 1; // at least one slide

    for sIdx := 0 to vSlides - 1 do
    begin
      // Add primary language
      if sIdx < vSlides - 1 then
      begin
        // For normal slide: Exactly MaxSlide as numbers
        for lIdx := sIdx * MaxSlides to (sIdx + 1) * MaxSlides - 1 do
          if lIdx < Lang1Lines.Count then
            OutputList.Add(Lang1Lines[lIdx]);
      end
      else
      begin
        // Last slide: All remaining lines
        for lIdx := sIdx * MaxSlides to Lang1Lines.Count - 1 do
          OutputList.Add(Lang1Lines[lIdx]);
      end;

      // Add delim
      if Lang2Lines.Count > 0 then
      begin
        OutputList.Add('---');
        if sIdx < vSlides - 1 then
        begin
          for lIdx := sIdx * MaxSlides to (sIdx + 1) * MaxSlides - 1 do
            if lIdx < Lang2Lines.Count then
              OutputList.Add(Lang2Lines[lIdx]);
        end
        else
        begin
          // Last slide takes all
          for lIdx := sIdx * MaxSlides to Lang2Lines.Count - 1 do
            OutputList.Add(Lang2Lines[lIdx]);
        end;
      end;

      // Empty Slide as Delimiter
      OutputList.Add('');
    end;

    Lang1Lines.Clear;
    Lang2Lines.Clear;
  end;

begin
  if (MaxSlides <= 0) or (Input.Count = 0) then
  begin
    Result := Input.Text;
    Exit;
  end;

  OutputList := TStringList.Create;
  Lang1Lines := TStringList.Create;
  Lang2Lines := TStringList.Create;
  try
    IsSecondLang := False;
    for i := 0 to Input.Count - 1 do
    begin
      Line := TrimRight(Input[i]); // Keep indents on the left

      if Line = '' then
      begin
        FlushVerse;
        IsSecondLang := False;
      end
      else if Line = '---' then
        IsSecondLang := True
      else
      begin
        if IsSecondLang then Lang2Lines.Add(Line)
        else Lang1Lines.Add(Line);
      end;
    end;
    FlushVerse;

    // Final Cleaning
    while (OutputList.Count > 0) and (OutputList[OutputList.Count - 1] = '') do
      OutputList.Delete(OutputList.Count - 1);

    Result := OutputList.Text;
  finally
    Lang1Lines.Free;
    Lang2Lines.Free;
    OutputList.Free;
  end;
end;

end.
