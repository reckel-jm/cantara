unit pptx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Slides, LCLType, Lyrics, ResourceHandling;

type
  TPPTXExporter = class
    public
      InputSongs: TSongList;
      SlideSettings: TSlideSettings;
      constructor Create; overload;
      destructor Destroy; override;
    private
      TempDir: String;
      pptxgenjs, template, exportedJs, content: TStringList;
      procedure AddSlide(Slide: TSlide);
      function SaveJavaScriptToFile: String;
  end;

implementation

function PrepareText(Input: String): String;
begin
  Result := StringReplace(Input, LineEnding, '\n', [rfReplaceAll]);
end;

constructor TPPTXExporter.Create;
begin
  inherited;
  TempDir := GetTempDir(False); // get the users temp dir
  // Load the Ressource Files here
  pptxgenjs := LoadResourceFileIntoStringList('PPTXGEN.BUNDLE');
  template := LoadResourceFileIntoStringList('PPTXEXPORT_TEMPLATE');
  content := TStringList.Create;
  exportedJs := TStringList.Create;
end;

procedure TPPTXExporter.AddSlide(Slide: TSlide);
begin
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
  end;
  if Slide.SlideType = EmptySlide then
  begin
    content.Add('slide = pres.addSlide({ masterName: "EmptySlide" });');
  end;
end;

function TPPTXExporter.SaveJavaScriptToFile: String;
var ExportedFilePath: String;
begin
  pptxgenjs.SaveToFile(TempDir + 'pptxgen.bundle.js');
  exportedJs.Text := StringReplace(template.Text, '{{SLIDECONTENT}}', content.Text, [rfReplaceAll]);
  ExportedFilePath := TempDir + 'pptx-export.html';
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

end.

