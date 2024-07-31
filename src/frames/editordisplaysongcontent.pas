unit editordisplaysongcontent;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Menus, Lyrics,
  Settings, ComCtrls, Dialogs, SynEdit, SynCompletion, SynMacroRecorder,
  SynPluginSyncroEdit, SynHighlighterAny, Types, LCLType, LConvEncoding,
  SynEditTypes;

type

  { TfrmDisplaySongContent }

  TfrmDisplaySongContent = class(TFrame)
    btnConvertCCLIFileToSongFormat: TButton;
    btnCopy: TButton;
    btnArchivate: TButton;
    btnClose: TButton;
    btnRename: TButton;
    btnSave: TButton;
    labelCCLIImportHint: TLabel;
    lblSongNameContent: TLabel;
    lblSongName: TLabel;
    ccliimporthint: TPanel;
    memoCode: TSynEdit;
    Save: TMenuItem;
    menuSong: TMenuItem;
    SynSongFormatHighlighter: TSynAnySyn;
    procedure btnArchivateClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    constructor Create(AOwner: TComponent);
    procedure btnCloseClick(Sender: TObject);
    procedure btnConvertCCLIFileToSongFormatClick(Sender: TObject);
    procedure lblSongNameContentDblClick(Sender: TObject);
    procedure memoCodeKeyPress(Sender: TObject; var Key: Char);
    procedure memoCodePaste(Sender: TObject; var AText: String;
      var AMode: TSynSelectionMode; ALogStartPos: TPoint;
      var AnAction: TSynCopyPasteAction);
    //  procedure lblSongNameContentDblClick(Sender: TObject);
  private
    procedure markAsChanged(FileHasChanged: Boolean);
  public
    openFile: TRepoFile;
    openFilePath: String;
    hasChanged: Boolean;
    procedure loadFile(repofile: TRepoFile);
    procedure saveFile;
    function RenameSongFile(newName: String): Boolean;
    //property OnFileChanged: TNotifyEvent read hasChanged write hasChanged;
  end;

resourcestring
  strFileCanNotBeRenamed =
    'The File can not be renamed. Make sure that you have the permissions to write to the song repository!';
  strErrorCCLIToSong =
    'The file can not be converted. Make sure that you have the permissions to write to the song repository!';
  StrEditSongNameCaption = 'Edit Song Name';
  StrEditSongNameContent = 'Please enter the new song name: ';

const
  ArchiveFolderName: String = 'archive';

implementation

uses
  songeditor;

  {$R *.lfm}

  { TfrmDisplaySongContent }

constructor TfrmDisplaySongContent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.hasChanged := False; // dont run markAsChanged as it may cause exceptions
end;

procedure TfrmDisplaySongContent.btnArchivateClick(Sender: TObject);
begin
  frmSongEdit.ArchivateCurrentTab;
end;

procedure TfrmDisplaySongContent.btnCopyClick(Sender: TObject);
begin
  frmSongEdit.CopyCurrentTab(openfile.Name);
end;

procedure TfrmDisplaySongContent.btnRenameClick(Sender: TObject);
begin
  lblSongNameContentDblClick(btnRename);
end;

procedure TfrmDisplaySongContent.btnSaveClick(Sender: TObject);
begin
  frmSongEdit.menuItemSaveClick(btnSave);
end;

procedure TfrmDisplaySongContent.btnConvertCCLIFileToSongFormatClick(Sender: TObject);
var
  song: TSong;
  SongFilePath: String;
begin
  song := TSong.Create;
  song.importSongfile(openFile.FilePath);
  SongFilePath := frmSettings.edtRepoPath.Text + PathDelim + openfile.Name + '.song';
  song.exportAsSongFile(SongFilePath);
  // move the old file to the ccli/-subfulder. If it does not exist, create it
  try
    if DirectoryExists(frmSettings.edtRepoPath.Text + PathDelim +
      ArchiveFolderName) = False then
      CreateDir(frmSettings.edtRepoPath.Text + PathDelim + ArchiveFolderName);
    if Not RenameFile(openFile.FilePath, frmSettings.edtRepoPath.Text +
      PathDelim + ArchiveFolderName + PathDelim + openfile.Name + '.ccli') then
      // move ccli file to subfolder
      ShowMessage(strErrorCCLIToSong);
    openfile.FileExtension := '.song';
    openfile.FileName := openfile.Name + openFile.FileExtension;
    openfile.FilePath := SongFilePath;
    LoadFile(openfile);
    frmSongEdit.loadRepoIntoSongListbox;
    markaschanged(False);
  except
    ShowMessage(strErrorCCLIToSong);
  end;
end;

procedure TfrmDisplaySongContent.btnCloseClick(Sender: TObject);
begin
  frmSongEdit.CloseCurrentTab;
end;

procedure TfrmDisplaySongContent.lblSongNameContentDblClick(Sender: TObject);
var
  NewSongName: String;
  InputQueryOK: Boolean;
begin
  NewSongName := OpenFile.Name;
  InputQueryOK := InputQuery(StrEditSongNameCaption, StrEditSongNameContent,
    NewSongName);
  if Not InputQueryOK then Exit;
  if RenameSongFile(NewSongName) then
    lblSongNameContent.Caption := NewSongName;
end;

procedure TfrmDisplaySongContent.memoCodeKeyPress(Sender: TObject; var Key: Char);
begin
  self.markAsChanged(True);
end;

procedure TfrmDisplaySongContent.memoCodePaste(Sender: TObject;
  var AText: String; var AMode: TSynSelectionMode; ALogStartPos: TPoint;
  var AnAction: TSynCopyPasteAction);
var
  GuessedCoding: String;
  Encoded: Boolean;
begin
  GuessedCoding := GuessEncoding(AText);
  AText := ConvertEncodingToUTF8(AText, GuessedCoding, Encoded);
end;

procedure TfrmDisplaySongContent.loadFile(repofile: TRepoFile);
var
  songimport: TSong; // needed for checking the type
begin
  self.openFile := repofile;
  self.openFilePath := repoFile.FilePath;
  memoCode.Lines.LoadFromFile(self.openFilePath);
  lblSongNameContent.Caption := openFile.Name;
  self.hasChanged := False; // dont run markAsChanged as it may cause exceptions
  { if CCLI File than show conversion suggestion instead of editor }
  songimport := TSong.Create;
  songimport.importSongFile(openfile.FilePath);
  ccliimporthint.Visible := songimport.IsCCLIFile;
  FreeAndNil(songimport);
end;

procedure TfrmDisplaySongContent.markAsChanged(FileHasChanged: Boolean);
var
  papa: TTabSheet;
begin
  self.hasChanged := FileHasChanged;
  papa := Owner As TTabSheet;
  if FileHasChanged then
  begin
    if pos(' [*]', Papa.Caption) = 0 then Papa.Caption := Papa.Caption + ' [*]';
  end
  else
    Papa.Caption := openFile.Name;
end;

procedure TfrmDisplaySongContent.SaveFile;
begin
  memoCode.Lines.SaveToFile(self.openFilePath);
  markAsChanged(False);
end;

function TfrmDisplaySongContent.RenameSongFile(newName: String): Boolean;
var
  newFilePath, fileExtension: String;
  changedoldstate: Boolean;
begin
  changedoldstate := hasChanged;
  // remember whether there are unsaved changes before the renaming
  FileExtension := ExtractFileExt(openFilePath);
  newFilePath := frmSettings.edtRepoPath.Text + PathDelim + newName + FileExtension;
  if (FileExists(NewFilePath)) Or (newName = '') Or
    (RenameFile(OpenFilePath, NewFilePath) = False) then
  begin
    ShowMessage(strFileCanNotBeRenamed);
    Exit(False);
  end;
  { Change all Variables of OpenFile Accordingly}
  openFile.Name := newName;
  openFile.FileName := newName + FileExtension;
  openFile.FileExtension := FileExtension;
  openFile.FilePath := newFilePath;
  frmSongEdit.loadRepoIntoSongListbox;
  self.loadFile(openfile);
  markAsChanged(changedoldstate);
  Result := True;
end;

end.
