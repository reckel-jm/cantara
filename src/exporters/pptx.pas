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
      pptxgenjs, template, example: TStringList;
  end;

implementation
constructor TPPTXExporter.Create;
begin
  inherited;
  TempDir := GetTempDir(False); // get the users temp dir
  // Load the Ressource Files here
  pptxgenjs := LoadResourceFileIntoStringList('PPTXGEN.BUNDLE');
  template := LoadResourceFileIntoStringList('PPTXEXPORT_TEMPLATE');
  example := LoadResourceFileIntoStringList('PPTXEXPORT_EXAMPLE');
  pptxgenjs.SaveToFile(TempDir + 'pptxgen.bundle.js');
  example.SaveToFile(TempDir + 'cantara-pptx-export.html');
end;

destructor TPPTXExporter.Destroy;
begin
  if Assigned(InputSongs) then FreeAndNil(InputSongs);
  FreeAndNil(pptxgenjs);
  FreeAndNil(template);
  FreeAndNil(example);
  inherited;
end;

end.

