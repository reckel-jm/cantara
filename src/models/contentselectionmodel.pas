unit contentselectionmodel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FGL, FileUtil,
  PresentationModels;

type
  TContentType = (NotAssigned, Song, Bible, Slide);

  { Represents a content file (song, etc.) which can be imported }

  { TContentFile }

  TContentFile = class(TObject)
  private
    fIsValid: Boolean;
    fFileEnding: String;
    fFilePath: String;
    procedure CheckValidity;
    procedure AssignContentType;
    procedure SetFilePath(AFilePath: String);
  public
    DisplayName: String;
    ContentType: TContentType;
    property FilePath: String read fFilePath write SetFilePath;
    property Valid: Boolean read fIsValid;
    constructor Create(AFilePath: String); overload;
    destructor Destroy; override;
  end;

  TContentFileList = specialize TFPGObjectList<TContentFile>;

  { This model represents the selection of content for a presentation or for
    an export. It covers the following elements:
     - importing a Song Repository
     - Selecting songs/content for a presentation
     - Switching the selected content order
     - Additional settings
  }

  { TRepositoryModel }

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

  TSelectedContentFile = class(TContentFile)
  public
    PresentationStyleSettings: PresentationModels.TPresentationStyleSettings;
  end;

  TSelectionList = specialize TFPGObjectList<TSelectedContentFile>;

  { TContentSelectionModel }

  TContentSelectionModel = class(TObject)
  private
  public
    DefaultPresentationStyleSettings: PresentationModels.TPresentationStyleSettings;
    SelectionList: TSelectionList;
    procedure SelectFile(AContentFile: TContentFile);
    constructor Create; overload;
    destructor Destroy; override;
  end;

const ALLOWED_FILEENDINGS:String = '*.song;*.ccli;*.txt';

implementation

{ TContentFile }

procedure TContentFile.CheckValidity;
begin
  if not FileExists(Self.fFilePath) then
  begin
    Self.fIsValid := False;
    Exit;
  end;
  if Self.ContentType = TContentType.Song then Self.fIsValid := true else
    Self.fIsValid := False;
end;

procedure TContentFile.AssignContentType;
begin
  if (Self.fFileEnding = '.song') or (Self.fFileEnding = '.ccli')
  or (Self.fFileEnding = '.txt') then
  begin
    Self.ContentType:=TContentType.Song;
  end;
end;

procedure TContentFile.SetFilePath(AFilePath: String);
begin
  Self.fFilePath:=AFilePath;
  Self.fFileEnding:=ExtractFileExt(AFilePath);
  Self.DisplayName:=ExtractFileName(Self.fFilePath);
  Self.DisplayName:=StringReplace(Self.DisplayName, Self.fFileEnding, '', [rfReplaceAll]);
end;

constructor TContentFile.Create(AFilePath: String);
begin
  Self.FilePath := AFilePath;
  Self.AssignContentType;
  Self.CheckValidity;
end;

destructor TContentFile.Destroy;
begin
  inherited Destroy;
end;

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

{ TContentSelectionModel }

procedure TContentSelectionModel.SelectFile(AContentFile: TContentFile);
var ASelectedContentFile: TSelectedContentFile;
begin
  ASelectedContentFile := TSelectedContentFile(AContentFile);
end;

constructor TContentSelectionModel.Create;
begin
  Self.SelectionList := TSelectionList.Create(False);
end;

destructor TContentSelectionModel.Destroy;
begin
  inherited Destroy;
  Self.SelectionList.Destroy;
end;

end.

