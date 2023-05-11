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
  Classes, SysUtils, Lyrics, fgl;

type

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
      Extra: String;
      ID: String;
      constructor Create; overload;
      destructor Destroy; override;

  end;

  TSlideList = specialize TFPGObjectList<TSlide>;

  { Create Presentation Data from a TSong with appropriate settings }
  function CreatePresentationDataFromSong(Song: TSong; SpoilerText: Boolean; FirstSlideMeta: Boolean; LastSlideMeta: Boolean; MetaSyntax: String; EmptyFrame: Boolean; MaxSlideLineLength: Integer): TSlideList;

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

  function CreatePresentationDataFromSong(Song: TSong; SpoilerText: Boolean; FirstSlideMeta: Boolean; LastSlideMeta: Boolean; MetaSyntax: String; EmptyFrame: Boolean; MaxSlideLineLength: Integer): TSlideList;
  var songfile: TStringList;
    completefilename: String;
    songname: string;
    stanza: string;
    CurrentSongSlideList: TSlideList;
    Slide: TSlide;
    j: Integer;
  begin
    Song.MaxSlideLineLength:=MaxSlideLineLength;
    Song.slideWrap;
    songfile := song.output;
    //gehe durch Songdatei und füge gleiche Strophen zu einem String zusammen
    stanza := '';
    CurrentSongSlideList := TSlideList.Create(False);
    for j := 0 to songfile.Count-1 do
    begin
      if (songfile.strings[j] = '') then
        begin
          Slide := TSlide.Create;
          Slide.Song := Song;
          Slide.PartContent.MainText:= stanza;
          stanza := '';
          CurrentSongSlideList.Add(Slide);
        end
        else stanza := stanza + songfile.Strings[j] + LineEnding;
    end;
    { Add the last stanza }
    Slide := TSlide.Create;
    Slide.Song := Song;
    Slide.PartContent.MainText:= stanza;
    CurrentSongSlideList.Add(Slide);
    { Add Spoiler Text to the slides if desired in the settings }

    if SpoilerText then
    begin
      for j := 0 to CurrentSongSlideList.Count-2 do
        CurrentSongSlideList.Items[j].PartContent.SpoilerText:=CurrentSongSlideList.Items[j+1].PartContent.MainText;
    end;

    { Add Meta Information to the slides if desired in the settings which were handed over }

    if (FirstSlideMeta) then
    begin
      CurrentSongSlideList.Items[0].PartContent.MetaText := Song.ParseMetaData(MetaSyntax);
    end;

    if (LastSlideMeta) then
    begin
      CurrentSongSlideList.Items[CurrentSongSlideList.Count-1].PartContent.MetaText := Song.ParseMetaData(MetaSyntax);
    end;

    { Add an empty frame if selected in the settings }
    if EmptyFrame then
    begin
      // We create a slide but with no content
      Slide := TSlide.Create;
      Slide.Song := Song;
      CurrentSongSlideList.Add(Slide);
    end;

    { Return CurrentSongSlideList }
    Result := CurrentSongSlideList;
  end;
end.

