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
      function GenerateBackgroundImageSettings(ImageFileEnding: String): String;
      function ImageToBase64: String;
  end;

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
    content.add('pres.addSection({ title: "' + Slide.Song.FileNameWithoutEnding + '" });');
    lastSongName := Slide.Song.FileNameWithoutEnding;
  end;
  if Slide.SlideType = SlideWithoutSpoiler then // this is a slide without spoiler
  begin
    content.Add('slide = pres.addSlide({ masterName: "SlideWithoutSpoiler" });');
    content.Add('slide.addText("' + PrepareText(Slide.PartContent.MainText) + '", { placeholder: "maincontent" })');
  end else
  if Slide.SlideType = SlideWithSpoiler then
  begin
    content.Add('slide = pres.addSlide({ masterName: "SlideWithSpoiler" });');
    content.Add('slide.addText("' + PrepareText(Slide.PartContent.MainText) + '", { placeholder: "maincontent" })');
    content.Add('slide.addText("' + PrepareText(Slide.PartContent.SpoilerText) + '", { placeholder: "spoiler" })');
  end else
  if Slide.SlideType = EmptySlide then
  begin
    content.Add('slide = pres.addSlide({ masterName: "EmptySlide" });');
  end else
  if Slide.SlideType = TitleSlide then
  begin
    content.Add('slide = pres.addSlide({ masterName: "TitleSlide" })');
    content.Add('slide.addText("' + PrepareText(Slide.PartContent.MainText) + '", { placeholder: "title" })');
  end;
  content.Add('slide.addNotes("' + PrepareText(Slide.PartContent.MainText) + '");');
end;

function TPPTXExporter.SaveJavaScriptToFile: String;
var ExportedFilePath: String;
ImageFileExtension: String;
begin
  pptxgenjs.SaveToFile(Folder + PathDelim + 'pptxgen.bundle.js');
  exportedJs.Text := StringReplace(template.Text, '{{SLIDECONTENT}}', content.Text, [rfReplaceAll]);
  exportedJs.Text := StringReplace(exportedJs.Text, '{{BACKGROUND}}', GenerateBackgroundColorSettings, [rfReplaceAll]);
  exportedJs.Text := StringReplace(exportedJs.Text, '{{TEXTCOLOR}}', ColorToHexString(PresentationStyleSettings.TextColor), [rfReplaceAll]);
  if PresentationStyleSettings.ShowBackgroundImage and FileExists(PresentationStyleSettings.BackgroundImageFilePath) then
  begin
    ImageFileExtension := ExtractFileExt(PresentationStyleSettings.BackgroundImageFilePath);
    CopyFile(PresentationStyleSettings.BackgroundImageFilePath,
      Folder + PathDelim + 'background' + ImageFileExtension);
    exportedJs.Text := StringReplace(exportedJs.Text, '{{BACKGROUNDIMAGE}}',
                    GenerateBackgroundImageSettings(ImageFileExtension), [rfReplaceAll]);
  end else
    exportedJs.Text := StringReplace(exportedJs.Text, '{{BACKGROUNDIMAGE}}', '', [rfReplaceAll]);
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

function TPPTXExporter.GenerateBackgroundImageSettings(ImageFileEnding: String): String;
begin
  if PresentationStyleSettings.ShowBackgroundImage then
     Result := Format('{ image: { x:0, y:0, w:10, h:5.6, data:"%s", transparancy: %s } }, ',
     [ImageToBase64, IntToStr(Abs(PresentationStyleSettings.Transparency))])
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
    Encoder.CopyFrom(TStringStream(imgstream), imgstream.Size);
    Encoder.Flush;
    Result:='data:image/jpg;base64,'+ TStringStream(Outputstream).DataString;
  finally
    imgstream.Free;
    Encoder.Free;
    Outputstream.Free;
    jpg.Free;
    OurPicture.Free;
  end;
end;

end.

