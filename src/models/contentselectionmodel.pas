unit contentselectionmodel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FGL, FileUtil,
  PresentationModels, SongTeX, textfilehandler, CantaraContentFile;

type


  { TRepositoryModel: This model represents the selection of content for a presentation or for
    an export. It covers the following elements:
     - importing a Song Repository
     - Selecting songs/content for a presentation
     - Switching the selected content order
     - Additional settings
  }

  TRepositoryModel = class(TObject)
  private
    fRepositoryFiles: TContentFileList;
    fRepositoryPath: String;
    fRepoFiles: TContentFileList;
    procedure SetRepositoryPath(ARepositoryPath: String);
    procedure ImportFilesFromPath(FilePaths: TStringList);
    function GetNumberOfFiles: Integer;
  public
    property RepositoryFiles: TContentFileList read fRepositoryFiles;
    property ItemCount: Integer read GetNumberOfFiles;
    property RepositoryPath: String read fRepositoryPath write SetRepositoryPath;
    procedure LoadRepository(ARepositoryPath: String);
    procedure UpdateRepository;
    constructor Create; overload;
    destructor Destroy; override;
  end;

  { TSelectedContentFile }

  TSelectedContentFile = class(TObject)
  public
    ContentFile: TContentFile;
    PresentationStyleSettings: PresentationModels.TPresentationStyleSettings;
    constructor Create(AContentFile: TContentFile);
  end;

  TSelectionList = specialize TFPGObjectList<TSelectedContentFile>;

  { TContentSelectionModel }

  TContentSelectionModel = class(TObject)
  private
  public
    DefaultPresentationStyleSettings: PresentationModels.TPresentationStyleSettings;
    SelectionList: TSelectionList;
    procedure SelectFile(AContentFile: TContentFile);
    procedure ExportSelectionAsSongTeXFile(AFileName: String);
    constructor Create; overload;
    destructor Destroy; override;
  end;

const ALLOWED_FILEENDINGS:String = '*.song;*.ccli;*.txt';

implementation

{ TRepositoryModel }

procedure TRepositoryModel.SetRepositoryPath(ARepositoryPath: String);
begin
  Self.fRepositoryPath:=ARepositoryPath;
end;

procedure TRepositoryModel.ImportFilesFromPath(FilePaths: TStringList);
var FilePath: String;
  ContentFile: TContentFile;
begin
  for FilePath in FilePaths do
  begin
    ContentFile := TContentFile.Create(FilePath);
    if ContentFile.Valid then // We only add valid files
    begin
      Self.fRepositoryFiles.Add(ContentFile);
    end
    // We ignore and destroy unvalid files
    else ContentFile.Destroy;
  end;
end;

function TRepositoryModel.GetNumberOfFiles: Integer;
begin
  Result := fRepositoryFiles.Count;
end;

procedure TRepositoryModel.LoadRepository(ARepositoryPath: String);
begin
  Self.RepositoryPath:=ARepositoryPath;
  Self.UpdateRepository;
end;

procedure TRepositoryModel.UpdateRepository;
var SearchResultFilePaths: TStringList;
begin
  Self.fRepositoryFiles.Clear;
  SearchResultFilePaths := TStringList.Create;
  FindAllFiles(SearchResultFilePaths, RepositoryPath, ALLOWED_FILEENDINGS, True);
  Self.ImportFilesFromPath(SearchResultFilePaths);
  SearchResultFilePaths.Destroy;
end;

constructor TRepositoryModel.Create;
begin
  fRepositoryFiles := TContentFileList.Create(True);
end;

destructor TRepositoryModel.Destroy;
begin
  inherited Destroy;
  Self.fRepositoryFiles.Destroy;
end;

{ TSelectedContentFile }

constructor TSelectedContentFile.Create(AContentFile: TContentFile);
begin
  Self.ContentFile := AContentFile;
end;

{ TContentSelectionModel }

procedure TContentSelectionModel.SelectFile(AContentFile: TContentFile);
var ASelectedContentFile: TSelectedContentFile;
begin
  ASelectedContentFile := TSelectedContentFile.Create(AContentFile);
  ASelectedContentFile.PresentationStyleSettings := Self.DefaultPresentationStyleSettings;
  Self.SelectionList.Add(ASelectedContentFile);
end;

procedure TContentSelectionModel.ExportSelectionAsSongTeXFile(AFileName: String);
var
  i: Integer;
  songtexfile: TSongTeXFile;
  song: TContentFile;
  songtexfilename: String;
  TextFileHandler: TTextFileHandler;
  TextFile: String;
begin
  TextFileHandler := TTextFileHandler.Create;
  songtexfile := TSongTeXFile.Create;
  for i := 0 to Self.SelectionList.Count - 1 do
  begin
    song := Self.SelectionList.Items[i].ContentFile;
    songtexfile.AddFile(song);
  end;
  songtexFileName := AFileName;
  if ExtractFileExt(AFileName) <> '.songtex' then
    songtexFileName := songtexFileName + '.songtex';
  TextFile := songtexfile.Text;
  TextFileHandler.SaveTextFile(TextFile, AFileName, '.songtex');
  TextFileHandler.Destroy;
end;

constructor TContentSelectionModel.Create;
begin
  Self.SelectionList := TSelectionList.Create(True);
end;

destructor TContentSelectionModel.Destroy;
begin
  inherited Destroy;
  Self.SelectionList.Destroy;
end;

end.

