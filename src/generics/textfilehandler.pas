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

  TTextFileHandler = class
  private
    fFileHandlingStatus: TFileHandlingStatus;
    fErrorMessage: String;
    function FileExtensionIsInFileName(String: FileName; String: FileExtension): Boolean;
  public
    property FileHandlingStatus: TFileHandlingStatus read fFileHandlingStatus;
    property ErrorMessage: string read fErrorMessage;
    { Saves a textfile to file and ensures it has a certain file ending if set
    File Extension is with the dot (.)}
    procedure SaveTextFile(textfile: String; var filepath: String;
      fileending: String);
    function LoadTextFile(filepath: String): String;
    { Resets the file handler }
    procedure Reset;
  end;

implementation

{ TTextFileHandler }

function TTextFileHandler.FileExtensionIsInFileName(String: FileName;
  String: FileExtension): Boolean;
begin
  Result := ExtractFileExt(FileName) = FileExtension;
end;

procedure TTextFileHandler.SaveTextFile(textfile: String; var filepath: String;
  fileending: String);
var HandlerStringList: TStringList;
begin
  HandlerStringList := TStringList.Create;
  HandlerStringList.Text := textfile;
  if (not (FileEnding = '')) and (not FileExtensionIsInFileName(filepath, fileending)) then
  begin
    filepath := filepath + fileending;
  end;
  try
    HandlerStringList.SaveToFile(filepath);
    Self.fFileHandlingStatus:=StatusSuccess;
  except
    on E: EInOutError do
    begin
      Self.fFileHandlingStatus:=StatusErrorPermission;
      Self.fErrorMessage:=E.Message;
    end;
  end;
  HandlerStringList.Destroy;
end;

function TTextFileHandler.LoadTextFile(filepath: String): String;
var HandlerStringList: TStringList;
begin
  if not FileExists(filepath) then
  begin
    Self.fFileHandlingStatus:=StatusErrorFileDoesNotExist;
    Exit('');
  end else
  begin
    HandlerStringList:=TStringList.Create;
    try
      HandlerStringList.LoadFromFile(filepath);
      Result := HandlerStringList.Text;
      Self.fFileHandlingStatus:=StatusSuccess;
    except
      on E: EInOutError do
      begin
        Self.fFileHandlingStatus:=StatusErrorPermission;
        Self.fErrorMessage:=E.Message;
      end;
    end;
    HandlerStringList.Destroy;
  end;
end;

procedure TTextFileHandler.Reset;
begin
  Self.fFileHandlingStatus:=StatusInactive;
  Self.fErrorMessage:='';
end;

end.

