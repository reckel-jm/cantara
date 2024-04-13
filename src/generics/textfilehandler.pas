{
  The unit textfilehandler contains the enum datatype TFileHandlingStatus and
  the class TTextFileHandler which is a safe wrapper around reading and
  writing text files.
}
unit textfilehandler;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Strings;

type
  TFileHandlingStatus = (StatusInactive, StatusSuccess,
    StatusErrorFileDoesNotExist, StatusErrorPermission);
  {
    This Class handles the save loading and saving of text files.
  }

  { TTextFileHandler }

  TTextFileHandler = class(TObject)
  private
    fFileHandlingStatus: TFileHandlingStatus;
    fErrorMessage: string;
    function FileExtensionIsInFileName(FileName: string; FileExtension: string): boolean;
  public
    property FileHandlingStatus: TFileHandlingStatus read fFileHandlingStatus;
    property ErrorMessage: string read fErrorMessage;
    { Saves a textfile to file and ensures it has a certain file ending if set
    File Extension is with the dot (.)}
    procedure SaveTextFile(textfile: string; var filepath: string;
      fileending: string);
    function OpenTextFile(filepath: string): string;
    { Resets the file handler }
    procedure Reset;
    constructor Create; overload;
    destructor Destroy; override;
  end;

implementation

{ TTextFileHandler }

function TTextFileHandler.FileExtensionIsInFileName(FileName: string;
  FileExtension: string): boolean;
begin
  Result := ExtractFileExt(FileName) = FileExtension;
end;

procedure TTextFileHandler.SaveTextFile(textfile: string; var filepath: string;
  fileending: string);
var
  HandlerStringList: TStringList;
begin
  HandlerStringList := TStringList.Create;
  HandlerStringList.Text := textfile;
  if (not (FileEnding = '')) and
    (not FileExtensionIsInFileName(filepath, fileending)) then
  begin
    filepath := filepath + fileending;
  end;
  try
    HandlerStringList.SaveToFile(filepath);
    Self.fFileHandlingStatus := StatusSuccess;
  except
    on E: EInOutError do
    begin
      Self.fFileHandlingStatus := StatusErrorPermission;
      Self.fErrorMessage := E.Message;
    end;
  end;
  HandlerStringList.Destroy;
end;

function TTextFileHandler.OpenTextFile(filepath: string): string;
var
  HandlerStringList: TStringList;
begin
  if not FileExists(filepath) then
  begin
    Self.fFileHandlingStatus := StatusErrorFileDoesNotExist;
    Exit('');
  end
  else
  begin
    HandlerStringList := TStringList.Create;
    try
      HandlerStringList.LoadFromFile(filepath);
      Result := HandlerStringList.Text;
      Self.fFileHandlingStatus := StatusSuccess;
    except
      on E: EInOutError do
      begin
        Self.fFileHandlingStatus := StatusErrorPermission;
        Self.fErrorMessage := E.Message;
      end;
    end;
    HandlerStringList.Destroy;
  end;
end;

procedure TTextFileHandler.Reset;
begin
  Self.fFileHandlingStatus := StatusInactive;
  Self.fErrorMessage := '';
end;

constructor TTextFileHandler.Create;
begin
  Self.Reset;
end;

destructor TTextFileHandler.Destroy;
begin
  inherited Destroy;
end;

end.
