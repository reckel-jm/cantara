{ <description>

  Copyright (C) 2023 Jan Martin Reckel <jm.reckel@t-online.de>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <https://www.gnu.org/licenses/gpl-3.0.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

  This unit uses pptxgenjs <https://pptxgenjs.github.io> which is published by
  Brent Ely under the terms and regulations of the MIT license.

  pptxgenjs: Copyright (c) 2015-Present Brent Ely

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
}

unit pptx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Slides, LCLType, Lyrics, ResourceHandling, PresentationCanvas,
  FileUtil, Graphics, Base64;

type
  TPPTXExporter = class
    public
      InputSongs: TSongList;
      PresentationStyleSettings: TPresentationStyleSettings;
      Folder: String;
      constructor Create; overload;
      destructor Destroy; override;
      function SaveJavaScriptToFile: String;
      procedure AddSlides(ASlideList: TSlideList);
    private
      pptxgenjs, template, exportedJs, content: TStringList;
      lastSongName: String;
      function GenerateBackgroundColorSettings: String;
      procedure AddSlide(Slide: TSlide);
      function ColorToHexString(AColor: TColor): String;
      function GenerateBackgroundImageSettings: String;
      function ImageToBase64: String;
  end;

const
  CodeAddSlide:String = 'slide = pres.addSlide({ masterName: "%s" });';
  CodeAddSpoileredText:String = 'slide.addText(' +
                                 '[' +
                                 '{ text: "%s\n\n", options: {} }, '+
                                 '{ text: "%s", options: { fontSize: "18" } },'+
                                 '],'+
                                 '{ placeholder: "defaultcontent" }'+
                                 ');';
  CodeAddTitleText:String = 'slide.addText("%s", { placeholder: "title" })';
  CodeAddUnspoileredText:String = 'slide.addText("%s", { placeholder: "defaultcontent" })';
  CodeAddSection:String = 'pres.addSection({ title: "%s" });';

ResourceString
  StrCantaraExport = 'Cantara Song Presentation Export';
  StrSongs = 'Songs';

implementation

function PrepareText(Input: String): String;
begin
  Input := Trim(Input);
  Result := StringReplace(Input, LineEnding, '\n', [rfReplaceAll]);
end;

constructor TPPTXExporter.Create;
begin
  inherited;
  // Load the Ressource Files here
  pptxgenjs := LoadResourceFileIntoStringList('PPTXGEN.BUNDLE');
  template := LoadResourceFileIntoStringList('PPTXEXPORT_TEMPLATE');
  content := TStringList.Create;
  exportedJs := TStringList.Create;
end;

procedure TPPTXExporter.AddSlide(Slide: TSlide);
begin
  if Slide.Song.FileNameWithoutEnding <> lastSongName then
  begin
    content.Add(Format(CodeAddSection, [PrepareText(Slide.Song.FileNameWithoutEnding)]));
    lastSongName := Slide.Song.FileNameWithoutEnding;
  end;
  if Slide.SlideType = SlideWithoutSpoiler then // this is a slide without spoiler
  begin
    content.Add(Format(CodeAddSlide, ['DefaultSlide']));
    content.Add(Format(CodeAddUnspoileredText, [PrepareText(Slide.PartContent.MainText)]));
  end else
  if Slide.SlideType = SlideWithSpoiler then
  begin
    content.Add(Format(CodeAddSlide, ['DefaultSlide']));
    content.Add(Format(CodeAddSpoileredText, [PrepareText(Slide.PartContent.MainText), PrepareText(Slide.PartContent.SpoilerText)]));
  end else
  if Slide.SlideType = EmptySlide then
  begin
    content.Add(Format(CodeAddSlide, ['EmptySlide']));
  end else
  if Slide.SlideType = TitleSlide then
  begin
    content.Add(Format(CodeAddSlide, ['TitleSlide']));
    content.Add(Format(CodeAddTitleText, [PrepareText(Slide.PartContent.MainText)]));
  end;
  content.Add('slide.addNotes("' + PrepareText(Slide.PartContent.MainText) + '");');
end;

function TPPTXExporter.SaveJavaScriptToFile: String;
var ExportedFilePath: String;
  halign, valign: String;
begin
  pptxgenjs.SaveToFile(Folder + PathDelim + 'pptxgen.bundle.js');
  exportedJs.Text := StringReplace(template.Text, '{{SLIDECONTENT}}', content.Text, [rfReplaceAll]);
  exportedJs.Text := StringReplace(exportedJs.Text, '{{BACKGROUND}}', GenerateBackgroundColorSettings, [rfReplaceAll]);
  exportedJs.Text := StringReplace(exportedJs.Text, '{{TEXTCOLOR}}', ColorToHexString(PresentationStyleSettings.TextColor), [rfReplaceAll]);
  if PresentationStyleSettings.ShowBackgroundImage and FileExists(PresentationStyleSettings.BackgroundImageFilePath) then
  begin
    exportedJs.Text := StringReplace(exportedJs.Text, '{{BACKGROUNDIMAGE}}',
                    GenerateBackgroundImageSettings, [rfReplaceAll]);
  end else
    exportedJs.Text := StringReplace(exportedJs.Text, '{{BACKGROUNDIMAGE}}', '', [rfReplaceAll]);

  { Add Meta-Data }
  exportedJs.Text := StringReplace(exportedJs.Text, '{{TITLE}}', StrCantaraExport, [rfReplaceAll]);
  exportedJs.Text := StringReplace(exportedJs.Text, '{{SUBJECT}}', StrSongs, [rfReplaceAll]);
  { Horizontal and Vertical Align }
  case PresentationStyleSettings.HorizontalAlign of
    Align_Left: halign := 'left';
    Align_Center: halign := 'center';
    Align_Right: halign := 'right';
  end;
  exportedJs.Text := StringReplace(exportedJs.Text, '{{HALIGN}}', halign, [rfReplaceAll]);
  case PresentationStyleSettings.VerticalAlign of
    tlTop   : valign := 'top';
    tlCenter: valign := 'middle';
    tlBottom: valign := 'bottom';
  end;
  exportedJs.Text := StringReplace(exportedJs.Text, '{{VALIGN}}', valign, [rfReplaceAll]);
  ExportedFilePath := Folder + PathDelim + 'pptx-export.html';
  exportedJs.SaveToFile(ExportedFilePath);
  Result := ExportedFilePath;
end;

destructor TPPTXExporter.Destroy;
begin
  if Assigned(InputSongs) then FreeAndNil(InputSongs);
  FreeAndNil(pptxgenjs);
  FreeAndNil(template);
  exportedJs.Destroy;
  content.Destroy;
  inherited;
end;

procedure TPPTXExporter.AddSlides(ASlideList: TSlideList);
var Slide: TSlide;
begin
  for Slide in ASlideList do
  begin
    AddSlide(Slide);
  end;
end;

function TPPTXExporter.GenerateBackgroundColorSettings: String;
begin
  Result := 'background: { color: "' +
         ColorToHexString(PresentationStyleSettings.BackgroundColor) + '" },';
end;

function TPPTXExporter.GenerateBackgroundImageSettings: String;
begin
  if PresentationStyleSettings.ShowBackgroundImage then
     Result := Format('{ image: { x:0, y:0, w:10, h:5.6, data:"%s" } }, ',
     [ImageToBase64])
  else Result := '';
end;

function TPPTXExporter.ColorToHexString(AColor: TColor): String;
var
R, G, B: Byte;
begin
  RedGreenBlue(AColor, R, G, B);
  Result := Format('%.2x%.2x%.2x', [R, G, B]);
end;

function TPPTXExporter.ImageToBase64: String;
var
  imgstream, Outputstream: TStream;
  Encoder: TBase64EncodingStream;
  jpg: TJPEGImage;
  I: Int64;
  OurPicture: TPicture;
  PresentationCanvas: TPresentationCanvasHandler;
begin
  OurPicture := TPicture.Create;
  OurPicture.LoadFromFile(PresentationStyleSettings.BackgroundImageFilePath);
  PresentationCanvas := TPresentationCanvasHandler.Create;
  PresentationCanvas.PresentationStyleSettings := PresentationStyleSettings;
  PresentationCanvas.Width:=OurPicture.Width;
  PresentationCanvas.Height:=Round(PresentationCanvas.Width/16*9);
  PresentationCanvas.LoadBackgroundBitmap;
  OurPicture.Bitmap.Assign(PresentationCanvas.ResizedBackgroundBitmap);
  I := GetTickCount64;
  imgstream := TMemoryStream.Create();
  Outputstream := TStringStream.Create('');
  jpg := TJPEGImage.Create;
  Encoder := TBase64EncodingStream.Create(Outputstream);
  try
    jpg.Assign(OurPicture.Bitmap);
    jpg.CompressionQuality:=75;
    jpg.SaveToStream(imgstream);
    imgstream.Position:= 0;
    Encoder.CopyFrom(imgstream, imgstream.Size);
    Encoder.Flush;
    Result:='data:image/jpg;base64,'+ (Outputstream as TStringStream).DataString;
  finally
    imgstream.Free;
    Encoder.Free;
    Outputstream.Free;
    jpg.Free;
    OurPicture.Free;
  end;
end;

end.

