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
{
  Splits a song's source text into multiple slides based on a maximum line count.
  Features:
  - Synchronized Bilingual Splitting: Keeps primary text and translation paired.
  - Balanced Distribution: Instead of 4+1 lines, it prefers a 3+2 split for better aesthetics.
  - Primary-Driven: The number of slides is dictated by the first language to avoid
    empty "translation-only" slides.
}
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
  var
    sIdx, lIdx, vSlides: Integer;
    LinesThisSlide1, LinesThisSlide2: Integer;
    Offset1, Offset2: Integer;
  begin
    // Skip if there is no content to process
    if (Lang1Lines.Count = 0) and (Lang2Lines.Count = 0) then Exit;

    { 1. CALCULATE SLIDE COUNT
      The number of slides (vSlides) is determined solely by the primary language (Lang1).
      Formula ensures that even if MaxSlides is not reached, at least 1 slide is created. }
    vSlides := (Lang1Lines.Count + MaxSlides - 1) div MaxSlides;
    if vSlides = 0 then vSlides := 1;

    Offset1 := 0;
    Offset2 := 0;

    { 2. DISTRIBUTE LINES ACROSS SLIDES }
    for sIdx := 0 to vSlides - 1 do
    begin
      { --- BALANCING LOGIC ---
        To avoid a single line on the last slide, we distribute lines evenly.
        Example: 5 lines with MaxSlides=4 results in 3 lines on slide 1 and 2 lines on slide 2. }

      // Calculate lines for Primary Language on this specific slide
      LinesThisSlide1 := (Lang1Lines.Count div vSlides);
      if sIdx < (Lang1Lines.Count mod vSlides) then Inc(LinesThisSlide1);

      // Calculate lines for Translation on this specific slide (distributed over same vSlides)
      LinesThisSlide2 := (Lang2Lines.Count div vSlides);
      if sIdx < (Lang2Lines.Count mod vSlides) then Inc(LinesThisSlide2);

      // Add Primary Language lines to the output
      for lIdx := 0 to LinesThisSlide1 - 1 do
      begin
        if Offset1 < Lang1Lines.Count then
        begin
          OutputList.Add(Lang1Lines[Offset1]);
          Inc(Offset1);
        end;
      end;

      // Add Separator and Translation lines if content exists for this slide
      if (Lang2Lines.Count > 0) and (LinesThisSlide2 > 0) then
      begin
        OutputList.Add('---');
        for lIdx := 0 to LinesThisSlide2 - 1 do
        begin
          if Offset2 < Lang2Lines.Count then
          begin
            OutputList.Add(Lang2Lines[Offset2]);
            Inc(Offset2);
          end;
        end;
      end;

      // Add an empty line to mark the end of a slide (internal structure)
      OutputList.Add('');
    end;

    // Clear temporary buffers for the next verse block
    Lang1Lines.Clear;
    Lang2Lines.Clear;
  end;

begin
  { PRE-FLIGHT CHECKS }
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

    { MAIN PARSING LOOP
      Identifies verse blocks and language separators (---) }
    for i := 0 to Input.Count - 1 do
    begin
      Line := TrimRight(Input[i]);

      if Line = '' then
      begin
        // Empty line indicates the end of a verse/stanza
        FlushVerse;
        IsSecondLang := False;
      end
      else if Line = '---' then
      begin
        // Separator indicates that subsequent lines belong to the translation
        IsSecondLang := True;
      end
      else
      begin
        // Sort line into the appropriate language buffer
        if IsSecondLang then Lang2Lines.Add(Line)
        else Lang1Lines.Add(Line);
      end;
    end;

    // Process the final block (in case the file doesn't end with an empty line)
    FlushVerse;

    { CLEANUP
      Remove any trailing empty lines from the generated output }
    while (OutputList.Count > 0) and (OutputList[OutputList.Count - 1] = '') do
      OutputList.Delete(OutputList.Count - 1);

    Result := OutputList.Text;
  finally
    // Ensure all temporary lists are freed to prevent memory leaks
    Lang1Lines.Free;
    Lang2Lines.Free;
    OutputList.Free;
  end;
end;

end.
