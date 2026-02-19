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
  FileUtil, Graphics, Base64, PresentationModels, fpjson;

type
  TPPTXExporter = class
  public
    PresentationStyleSettings: TPresentationStyleSettings;
    Folder: String;
    constructor Create; overload;
    destructor Destroy; override;
    function SaveJavaScriptToFile: String;
    procedure AddSlides(ASlideList: TSlideList);
  private
    pptxgenjs, template, exportedJs, content: TStringList;
    lastSongName: String;
    FCustomMasterDefs: TStringList;
    FCustomStyleKeys: TStringList;
    FCustomMasterCount: Integer;
    function GenerateBackgroundColorSettings: String;
    procedure AddSlide(Slide: TSlide);
    function ColorToHexString(AColor: TColor): String;
    function GenerateBackgroundImageSettings: String;
    function ImageToBase64: String;
    function StyleFingerprint(const AStyle: TPresentationStyleSettings): String;
    function ImageToBase64ForStyle(const AStyle: TPresentationStyleSettings): String;
    function GenerateMasterDef(const MasterTitle, PlaceholderName: String;
      const AStyle: TPresentationStyleSettings; const ABgImage: String): String;
    function GetOrCreateCustomMasterBaseName(
      const AStyle: TPresentationStyleSettings): String;
  end;

const
  CodeAddSlide: String = 'slide = pres.addSlide({ masterName: "%s" });';
  CodeAddSpoileredText: String =
    'slide.addText(' + '[' + '{ text: "%s\n", options: { align: "{{HALIGN}}" } }, ' +
    '{ text: "%s", options: { fontSize: "18" }, align: "{{HALIGN}}" },' +
    '],' + '{ placeholder: "defaultcontent" }' + ');';
  CodeAddTitleText: String =
    'slide.addText("%s", { placeholder: "title", align: "{{HALIGN}}" })';
  CodaAddTitleTextWithMetaData: String =
    'slide.addText(' + '[' +
    '{ text: "%s\n", options: { bold: true, align: "{{HALIGN}}" } }, ' +
    '{ text: "%s", options: { fontSize: "18", bold: false, align: "{{HALIGN}}" } },' +
    '],' + '{ placeholder: "title" }' + ');';
  CodeAddUnspoileredText: String =
    'slide.addText("%s", { placeholder: "defaultcontent" });';
  CodeAddSection: String = 'pres.addSection({ title: "%s" });';
  CodeAddMetaText: String =
    'slide.addText("[text]", { x: 0.15, y: "90%", h: "6%", valign: "bottom", color: "{{TEXTCOLOR}}", fontSize: "12", autoFit: true });';

resourcestring
  StrCantaraExport = 'Cantara Song Presentation Export';
  StrSongs = 'Songs';

implementation

function PrepareText(Input: String): String;
begin
  Input := Trim(Input);
  Result := StringToJSONString(Input);
  Result := StringReplace(Result, LineEnding, '\n', [rfReplaceAll]);
end;

constructor TPPTXExporter.Create;
begin
  inherited;
  // Load the Ressource Files here
  pptxgenjs := LoadResourceFileIntoStringList('PPTXGEN.BUNDLE');
  template := LoadResourceFileIntoStringList('PPTXEXPORT_TEMPLATE');
  content := TStringList.Create;
  exportedJs := TStringList.Create;
  FCustomMasterDefs := TStringList.Create;
  FCustomStyleKeys := TStringList.Create;
  FCustomMasterCount := 0;
end;

procedure TPPTXExporter.AddSlide(Slide: TSlide);
var
  MasterPrefix: String;
begin
  if Slide.Song.FileNameWithoutEnding <> lastSongName then
  begin
    content.Add(Format(CodeAddSection, [PrepareText(Slide.Song.FileNameWithoutEnding)]));
    lastSongName := Slide.Song.FileNameWithoutEnding;
  end;
  if Slide.HasCustomStyle then
    MasterPrefix := GetOrCreateCustomMasterBaseName(Slide.CustomStyle) + '_'
  else
    MasterPrefix := '';
  if Slide.SlideType = SlideWithoutSpoiler then // this is a slide without spoiler
  begin
    content.Add(Format(CodeAddSlide, [MasterPrefix + 'DefaultSlide']));
    content.Add(Format(CodeAddUnspoileredText,
      [PrepareText(Slide.PartContent.MainText)]));
  end
  else
  if Slide.SlideType = SlideWithSpoiler then
  begin
    content.Add(Format(CodeAddSlide, [MasterPrefix + 'DefaultSlide']));
    content.Add(Format(CodeAddSpoileredText,
      [PrepareText(Slide.PartContent.MainText),
      PrepareText(Slide.PartContent.SpoilerText)]));
  end
  else
  if Slide.SlideType = EmptySlide then
  begin
    content.Add(Format(CodeAddSlide, [MasterPrefix + 'EmptySlide']));
  end
  else
  if Slide.SlideType = TitleSlide then
  begin
    content.Add(Format(CodeAddSlide, [MasterPrefix + 'TitleSlide']));
    if Trim(Slide.PartContent.SpoilerText) = '' then
      content.Add(Format(CodeAddTitleText, [PrepareText(Slide.PartContent.MainText)]))
    else
      content.Add(Format(CodaAddTitleTextWithMetaData,
        [PrepareText(Slide.PartContent.MainText),
        PrepareText(Slide.PartContent.SpoilerText)])
        );
  end;
  if Trim(Slide.PartContent.MetaText) <> '' then
    content.Add(StringReplace(CodeAddMetaText, '[text]',
      PrepareText(Slide.PartContent.MetaText), [rfReplaceAll]));
  content.Add('slide.addNotes("' + PrepareText(Slide.PartContent.MainText) + '");');
end;

function TPPTXExporter.SaveJavaScriptToFile: String;
var
  ExportedFilePath: String;
  halign, valign: String;
  allContent: TStringList;
begin
  pptxgenjs.SaveToFile(Folder + PathDelim + 'pptxgen.bundle.js');
  allContent := TStringList.Create;
  try
    allContent.AddStrings(FCustomMasterDefs);
    allContent.AddStrings(content);
    exportedJs.Text := StringReplace(template.Text, '{{SLIDECONTENT}}',
      allContent.Text, [rfReplaceAll]);
  finally
    allContent.Free;
  end;
  exportedJs.Text := StringReplace(exportedJs.Text, '{{BACKGROUND}}',
    GenerateBackgroundColorSettings, [rfReplaceAll]);
  exportedJs.Text := StringReplace(exportedJs.Text, '{{TEXTCOLOR}}',
    ColorToHexString(PresentationStyleSettings.TextColor), [rfReplaceAll]);
  if PresentationStyleSettings.ShowBackgroundImage And
    FileExists(PresentationStyleSettings.BackgroundImageFilePath) then
  begin
    exportedJs.Text := StringReplace(exportedJs.Text, '{{BACKGROUNDIMAGE}}',
      GenerateBackgroundImageSettings, [rfReplaceAll]);
  end
  else
    exportedJs.Text := StringReplace(exportedJs.Text, '{{BACKGROUNDIMAGE}}',
      '', [rfReplaceAll]);

  { Add Meta-Data }
  exportedJs.Text := StringReplace(exportedJs.Text, '{{TITLE}}',
    StrCantaraExport, [rfReplaceAll]);
  exportedJs.Text := StringReplace(exportedJs.Text, '{{SUBJECT}}',
    StrSongs, [rfReplaceAll]);
  { Horizontal and Vertical Align }
  case PresentationStyleSettings.HorizontalAlign of
    Align_Left: halign := 'left';
    Align_Center: halign := 'center';
    Align_Right: halign := 'right';
  end;
  exportedJs.Text := StringReplace(exportedJs.Text, '{{HALIGN}}',
    halign, [rfReplaceAll]);
  case PresentationStyleSettings.VerticalAlign of
    tlTop: valign := 'top';
    tlCenter: valign := 'middle';
    tlBottom: valign := 'bottom';
  end;
  exportedJs.Text := StringReplace(exportedJs.Text, '{{VALIGN}}',
    valign, [rfReplaceAll]);
  ExportedFilePath := Folder + PathDelim + 'pptx-export.html';
  exportedJs.SaveToFile(ExportedFilePath);
  Result := ExportedFilePath;
end;

destructor TPPTXExporter.Destroy;
begin
  FreeAndNil(pptxgenjs);
  FreeAndNil(template);
  FreeAndNil(FCustomMasterDefs);
  FreeAndNil(FCustomStyleKeys);
  exportedJs.Destroy;
  content.Destroy;
  inherited;
end;

procedure TPPTXExporter.AddSlides(ASlideList: TSlideList);
var
  Slide: TSlide;
begin
  for Slide In ASlideList do
  begin
    AddSlide(Slide);
  end;
end;

function TPPTXExporter.GenerateBackgroundColorSettings: String;
begin
  Result := 'background: { color: "' + ColorToHexString(
    PresentationStyleSettings.BackgroundColor) + '" },';
end;

function TPPTXExporter.GenerateBackgroundImageSettings: String;
begin
  if PresentationStyleSettings.ShowBackgroundImage then
    Result := Format('{ image: { x:0, y:0, w:%s, h:%s, data:"%s" } }, ',
      ['"100%"', '"100%"', ImageToBase64])
  else
    Result := '';
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
  PresentationCanvas.Width := OurPicture.Width;
  PresentationCanvas.Height := Round(PresentationCanvas.Width / 16 * 9);
  PresentationCanvas.LoadBackgroundBitmap;
  OurPicture.Bitmap.Assign(PresentationCanvas.ResizedBackgroundBitmap);
  I := GetTickCount64;
  imgstream := TMemoryStream.Create();
  Outputstream := TStringStream.Create('');
  jpg := TJPEGImage.Create;
  Encoder := TBase64EncodingStream.Create(Outputstream);
  try
    jpg.Assign(OurPicture.Bitmap);
    jpg.CompressionQuality := 75;
    jpg.SaveToStream(imgstream);
    imgstream.Position := 0;
    Encoder.CopyFrom(imgstream, imgstream.Size);
    Encoder.Flush;
    Result := 'data:image/jpg;base64,' + (Outputstream As TStringStream).DataString;
  finally
    imgstream.Free;
    Encoder.Free;
    Outputstream.Free;
    jpg.Free;
    OurPicture.Free;
  end;
end;

function TPPTXExporter.StyleFingerprint(const AStyle: TPresentationStyleSettings): String;
begin
  Result := ColorToHexString(AStyle.BackgroundColor) + '|' +
             ColorToHexString(AStyle.TextColor) + '|' +
             BoolToStr(AStyle.ShowBackgroundImage, True) + '|' +
             AStyle.BackgroundImageFilePath + '|' +
             IntToStr(AStyle.Transparency) + '|' +
             IntToStr(Ord(AStyle.HorizontalAlign)) + '|' +
             IntToStr(Ord(AStyle.VerticalAlign)) + '|' +
             AStyle.Font.Name + '|' +
             IntToStr(Abs(AStyle.Font.Size));
end;

function TPPTXExporter.ImageToBase64ForStyle(const AStyle: TPresentationStyleSettings): String;
var
  imgstream, Outputstream: TStream;
  Encoder: TBase64EncodingStream;
  jpg: TJPEGImage;
  OurPicture: TPicture;
  PresentationCanvas: TPresentationCanvasHandler;
begin
  Result := '';
  if not FileExists(AStyle.BackgroundImageFilePath) then Exit;
  OurPicture := TPicture.Create;
  PresentationCanvas := TPresentationCanvasHandler.Create;
  try
    OurPicture.LoadFromFile(AStyle.BackgroundImageFilePath);
    PresentationCanvas.PresentationStyleSettings := AStyle;
    PresentationCanvas.Width := OurPicture.Width;
    PresentationCanvas.Height := Round(PresentationCanvas.Width / 16 * 9);
    PresentationCanvas.LoadBackgroundBitmap;
    OurPicture.Bitmap.Assign(PresentationCanvas.ResizedBackgroundBitmap);
    imgstream := TMemoryStream.Create();
    Outputstream := TStringStream.Create('');
    jpg := TJPEGImage.Create;
    Encoder := TBase64EncodingStream.Create(Outputstream);
    try
      jpg.Assign(OurPicture.Bitmap);
      jpg.CompressionQuality := 75;
      jpg.SaveToStream(imgstream);
      imgstream.Position := 0;
      Encoder.CopyFrom(imgstream, imgstream.Size);
      Encoder.Flush;
      Result := 'data:image/jpg;base64,' + (Outputstream As TStringStream).DataString;
    finally
      imgstream.Free;
      Encoder.Free;
      Outputstream.Free;
      jpg.Free;
    end;
  finally
    OurPicture.Free;
    PresentationCanvas.Free;
  end;
end;

function TPPTXExporter.GenerateMasterDef(const MasterTitle, PlaceholderName: String;
  const AStyle: TPresentationStyleSettings; const ABgImage: String): String;
var
  bgColor, textColor, halign, valign, fontSize, fontFace: String;
begin
  bgColor := 'background: { color: "' + ColorToHexString(AStyle.BackgroundColor) + '" },';
  textColor := ColorToHexString(AStyle.TextColor);
  case AStyle.HorizontalAlign of
    Align_Left:   halign := 'left';
    Align_Center: halign := 'center';
    Align_Right:  halign := 'right';
  else
    halign := 'center';
  end;
  case AStyle.VerticalAlign of
    tlTop:    valign := 'top';
    tlCenter: valign := 'middle';
    tlBottom: valign := 'bottom';
  else
    valign := 'middle';
  end;
  if Assigned(AStyle.Font) then
  begin
    fontFace := AStyle.Font.Name;
    if Abs(AStyle.Font.Size) > 0 then
      fontSize := IntToStr(Abs(AStyle.Font.Size))
    else
      fontSize := '32';
  end
  else
  begin
    fontFace := 'Calibri';
    fontSize := '32';
  end;
  Result :=
    'pres.defineSlideMaster({' + LineEnding +
    '  title: "' + MasterTitle + '",' + LineEnding +
    '  ' + bgColor + LineEnding +
    '  objects: [' + LineEnding +
    '    ' + ABgImage + LineEnding +
    '    { placeholder: {' + LineEnding +
    '      options: { name: "' + PlaceholderName + '", type: "body", ' +
    'x: 0, y: 0, w: "100%", h: "100%", align: "' + halign + '", ' +
    'valign: "' + valign + '", fontSize: "' + fontSize + '", ' +
    'fontFace: "' + fontFace + '", color: "' + textColor + '", ' +
    'margin: 15, autoFit: true },' + LineEnding +
    '      text: "",' + LineEnding +
    '    } },' + LineEnding +
    '  ]' + LineEnding +
    '});';
end;

function TPPTXExporter.GetOrCreateCustomMasterBaseName(
  const AStyle: TPresentationStyleSettings): String;
var
  Fingerprint, bgImage: String;
  Idx: Integer;
begin
  Fingerprint := StyleFingerprint(AStyle);
  Idx := FCustomStyleKeys.IndexOf(Fingerprint);
  if Idx >= 0 then
    Result := 'Custom' + IntToStr(Integer(FCustomStyleKeys.Objects[Idx]))
  else
  begin
    Inc(FCustomMasterCount);
    FCustomStyleKeys.AddObject(Fingerprint, TObject(PtrInt(FCustomMasterCount)));
    Result := 'Custom' + IntToStr(FCustomMasterCount);
    // Compute background image base64 once for all three masters
    if AStyle.ShowBackgroundImage and FileExists(AStyle.BackgroundImageFilePath) then
      bgImage := Format('{ image: { x:0, y:0, w:%s, h:%s, data:"%s" } }, ',
        ['"100%"', '"100%"', ImageToBase64ForStyle(AStyle)])
    else
      bgImage := '';
    FCustomMasterDefs.Add(GenerateMasterDef(
      Result + '_DefaultSlide', 'defaultcontent', AStyle, bgImage));
    FCustomMasterDefs.Add(GenerateMasterDef(
      Result + '_TitleSlide', 'title', AStyle, bgImage));
    FCustomMasterDefs.Add(GenerateMasterDef(
      Result + '_EmptySlide', 'emptycontent', AStyle, bgImage));
  end;
end;

end.
