unit pptx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Slides, LCLType, Lyrics;

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

Function LoadResourceFileIntoStringList(ResourceName: String): TStringList;

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

Function LoadResourceFileIntoStringList(ResourceName: String): TStringList;
var ResourceStream: TResourceStream;
begin
  ResourceStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  Result := TStringList.Create;
  Result.LoadFromStream(ResourceStream);
  FreeAndNil(ResourceStream);
end;

end.

