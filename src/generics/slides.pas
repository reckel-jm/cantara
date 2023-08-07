{ Unit for handling the logic of the slides in Cantara

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
  Classes, SysUtils, Lyrics, fgl, Dialogs;

type

  SlideTypeEnum = (TitleSlide, SlideWithSpoiler, SlideWithoutSpoiler, EmptySlide);

  { This record has all the settings required for a presentation slide }
  TSlideSettings = record
    SpoilerText: Boolean;
    TitleSlide: Boolean;
    FirstSlideMeta: Boolean;
    LastSlideMeta: Boolean;
    MetaSyntax: String;
    EmptyFrame: Boolean;
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
      constructor Create; overload;
      destructor Destroy; override;

  end;

  TSlideList = specialize TFPGObjectList<TSlide>;

  { Create Presentation Data from a TSong with appropriate settings }
  function CreatePresentationDataFromSong(Song: TSong; SlideSettings: TSlideSettings; var SlideCounter: Integer): TSlideList;
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
  end;

  destructor TSlide.Destroy;
  begin
    FreeAndNil(self.PartContent);
    inherited;
  end;

  function IsBilingual(var MainPart: String): String;
  var
    LanguageSeperatorPosition: Integer;
    SecondLanguagePart: String;
  begin
    LanguageSeperatorPosition := Pos('---', MainPart);
    if (LanguageSeperatorPosition > 0) and (Length(MainPart) > LanguageSeperatorPosition + 3) then
    begin
      SecondLanguagePart := Copy(MainPart, LanguageSeperatorPosition+3,
      Length(MainPart)-LanguageSeperatorPosition-2+1);
      MainPart := Trim(Copy(MainPart, 1, LanguageSeperatorPosition-1));
    end else
    SecondLanguagePart := '';
    Result := Trim(SecondLanguagePart);
  end;

  function CreatePresentationDataFromSong(Song: TSong; SlideSettings: TSlideSettings; var SlideCounter: Integer): TSlideList;
  var songfile: TStringList;
    completefilename: String;
    songname: string;
    stanza: string;
    slidestring: string;
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
    for j := 0 to songfile.Count-1 do
    begin
      if (songfile.strings[j] = '') then
        begin
          Slide := TSlide.Create;
          Slide.Song := Song;
          Slide.PartContent.MainText:= Trim(stanza);
          Slide.SlideType:=SlideWithoutSpoiler;
          Slide.ID := SlideCounter;
          SlideCounter += 1;
          stanza := '';
          { Find out whether song is billingual }
          SecondLanguageText := IsBilingual(Slide.PartContent.MainText);
          if SecondLanguageText <> '' then
          begin
            Slide.PartContent.SpoilerText:=SecondLanguageText;
            Slide.SlideType:=SlideWithSpoiler;
          end;

          CurrentSongSlideList.Add(Slide);
        end
        else stanza := stanza + songfile.Strings[j] + LineEnding;
    end;
    { Add the last stanza }
    Slide := TSlide.Create;
    Slide.Song := Song;
    Slide.PartContent.MainText:= Trim(stanza);
    Slide.ID := SlideCounter;
    Slide.SlideType:=SlideWithoutSpoiler;
    inc(SlideCounter);

    { Find out whether song is billingual }
    SecondLanguageText := IsBilingual(Slide.PartContent.MainText);
    if SecondLanguageText <> '' then
    begin
      Slide.PartContent.SpoilerText:=SecondLanguageText;
      Slide.SlideType:=SlideWithSpoiler;
    end;

    CurrentSongSlideList.Add(Slide);

    { Add Spoiler Text to the slides if desired in the settings }

    if SlideSettings.SpoilerText then
    begin
      for j := 0 to CurrentSongSlideList.Count-2 do
        if CurrentSongSlideList.Items[j].PartContent.SpoilerText = '' then
        begin
          CurrentSongSlideList.Items[j].PartContent.SpoilerText:=Trim(CurrentSongSlideList.Items[j+1].PartContent.MainText);
          CurrentSongSlideList.Items[j].SlideType:=SlideWithSpoiler;
        end;
    end;

    { Add Meta Information to the slides if desired in the settings which were handed over }

    if (SlideSettings.FirstSlideMeta) then
    begin
      CurrentSongSlideList.Items[0].PartContent.MetaText := Song.ParseMetaData(SlideSettings.MetaSyntax);
    end;

    if (SlideSettings.LastSlideMeta) then
    begin
      CurrentSongSlideList.Items[CurrentSongSlideList.Count-1].PartContent.MetaText := Song.ParseMetaData(SlideSettings.MetaSyntax);
    end;

    { Add an empty frame if selected in the settings }
    if SlideSettings.EmptyFrame then
    begin
      // We create a slide but with no content
      Slide := TSlide.Create;
      Slide.Song := Song;
      Slide.ID := SlideCounter;
      Slide.SlideType:=EmptySlide;
      inc(SlideCounter);
      CurrentSongSlideList.Add(Slide);
    end;

    { add title slide if desired in the settings }
    // We are doing that at the last slide because we want to avoid confusion with firstslide/lastslide BEFOREHAND

    if SlideSettings.TitleSlide then
    begin
      Slide := TSlide.Create;
      Slide.Song := Song;
      Slide.PartContent.MainText:=Trim(Song.MetaDict['title']);
      Slide.PartContent.SpoilerText:=Trim(Song.ParseMetaData(SlideSettings.MetaSyntax));
      Slide.SlideType:=TitleSlide;
      CurrentSongSlideList.Insert(0, Slide);
    end;
    { Clean Up }
    SongFile.Destroy;
    { Return CurrentSongSlideList }
    Result := CurrentSongSlideList;
  end;

function SplitSlides(Input: TStringList; MaxSlides: Integer): String;
var n1, n2,i,t,place: integer;
  changed: boolean;
  output: TStringList;
begin
  output := TStringList.Create;
  output.Assign(input);
  repeat
  begin
  changed := False;
  if MaxSlides <= 0 then exit; // Just as a protective measure, actually not needed anymore.
  if MaxSlides = 1 then       // it means to have one line per slide, so we take a shortpath
  begin
    i := 0;
    while i < output.count-1 do
      begin
        if (output.Strings[i] <> '') and (output.Strings[i] <> '---') then
          output.Insert(i+1, '');
        i := i+1;
      end;
    Break;
  end;
  n1 := 0;
  n2 := 0;
  for i := 0 to output.Count-1 do
  begin
    if (output.Strings[i] = '') or (output.Strings[i] = '---') then
    begin
       n2 := i;
       if (n2-n1) > MaxSlides then
         begin
           if MaxSlides mod 2 = 0 then
             place := (n1+(n2-n1) div 2)
           else place := (n1+(n2-n1) div 2) + 1;
           output.Insert(place, '');
           if (output.Strings[i+1] = '---') then
           begin
             if output.count > n2+1+place-n1 then
             begin
               output.Strings[place] := '---';
               for t := 0 to place-n1 do
               begin
                 output.Move(n2+1+t, place+t);
               end;
               output.Strings[n2+1] := '';
               output.Strings[n2+1+place-n1] := '---';
             end;
           end;
           changed := True;
         end;
       n1 := n2+1;
    end;
  end;
  // For the last slide
  n2 := output.Count;
  if (n2-n1) > MaxSlides then
     begin
         if MaxSlides mod 2 = 0 then
           output.Insert((n1+(n2-n1) div 2), '')
         else output.Insert((n1+(n2-n1) div 2) + 1, '');
         changed := True;
     end;
  end until changed = False;
  output.Text:=StringReplace(output.Text, LineEnding+LineEnding+LineEnding, LineEnding+LineEnding, [rfReplaceAll]);
  Result := output.Text;
  ShowMessage(output.Text);
  Output.Destroy;
end;

end.

