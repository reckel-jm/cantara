unit cantaracontentfile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FGL;

type

TContentType = (NotAssigned, Song, Bible, Slide);

{

TContentFile
Represents a content file (song, etc.) which can be imported

}

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

end.

