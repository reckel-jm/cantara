unit editordisplaysongcontent;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Menus, Lyrics,
  Settings, ComCtrls, Dialogs;

type

  { TfrmDisplaySongContent }

  TfrmDisplaySongContent = class(TFrame)
    edtSongName: TEdit;
    lblSongNameContent: TLabel;
    lblSongName: TLabel;
    memoCode: TMemo;
    Save: TMenuItem;
    menuSong: TMenuItem;
    constructor Create(AOwner: TComponent);
    procedure edtSongNameEditingDone(Sender: TObject);
    procedure edtSongNameExit(Sender: TObject);
    procedure edtSongNameMouseLeave(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure lblSongNameContentClick(Sender: TObject);
    procedure lblSongNameContentDblClick(Sender: TObject);
    procedure memoCodeChange(Sender: TObject);
    procedure memoCodeKeyPress(Sender: TObject; var Key: char);
    procedure menuSongClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
   //  procedure lblSongNameContentDblClick(Sender: TObject);
  private
    procedure markAsChanged(FileHasChanged: Boolean);
  public
    openFile: TRepoFile;
    openFilePath: String;
    hasChanged: Boolean;
    procedure loadFile(repofile: TRepoFile);
    procedure saveFile;
    procedure RenameSongFile(newName: String);
    //property OnFileChanged: TNotifyEvent read hasChanged write hasChanged;
  end;

ResourceString
  strFileCanNotBeRenamed = 'The File can not be renamed';

implementation

{$R *.lfm}

{ TfrmDisplaySongContent }

constructor TfrmDisplaySongContent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.hasChanged := False; // dont run markAsChanged as it may cause exceptions
end;

procedure TfrmDisplaySongContent.edtSongNameEditingDone(Sender: TObject);
begin
  edtSongNameExit(Sender);
end;

procedure TfrmDisplaySongContent.edtSongNameExit(Sender: TObject);
begin
  edtSongName.Visible:=False;
  if lblSongNameContent.Caption <> edtSongName.Text then
  begin
    lblSongNameContent.Caption:=edtSongName.Text;
    self.markAsChanged(True);
  end;
end;

procedure TfrmDisplaySongContent.edtSongNameMouseLeave(Sender: TObject);
begin
  edtSongName.Visible:=False;
  lblSongNameContent.Caption:=edtSongName.Text;
end;

procedure TfrmDisplaySongContent.FrameClick(Sender: TObject);
begin

end;

procedure TfrmDisplaySongContent.lblSongNameContentClick(Sender: TObject);
begin

end;

procedure TfrmDisplaySongContent.menuSongClick(Sender: TObject);
begin

end;

procedure TfrmDisplaySongContent.SaveClick(Sender: TObject);
begin

end;

procedure TfrmDisplaySongContent.lblSongNameContentDblClick(Sender: TObject);
begin
  edtSongName.Visible := True;
  edtSongName.Text := lblSongNameContent.Caption;
end;

procedure TfrmDisplaySongContent.memoCodeChange(Sender: TObject);
begin

end;

procedure TfrmDisplaySongContent.memoCodeKeyPress(Sender: TObject; var Key: char
  );
begin
  self.markAsChanged(True);
end;

procedure TfrmDisplaySongContent.loadFile(repofile: TRepoFile);
begin
  self.openFile := repofile;
  self.openFilePath := frmSettings.edtRepoPath.Text + PathDelim + repoFile.FileName;
  memoCode.Lines.LoadFromFile(self.openFilePath);
  lblSongNameContent.Caption:=openFile.Name;
  edtSongName.Text:=openFile.Name;
  self.hasChanged := False; // dont run markAsChanged as it may cause exceptions
end;

procedure TfrmDisplaySongContent.markAsChanged(FileHasChanged: Boolean);
var papa: TTabSheet;
begin
  self.hasChanged:=FileHasChanged;
  papa := Owner as TTabSheet;
  if FileHasChanged then
  begin
    if pos(' [*]', Papa.Caption) = 0 then Papa.Caption := Papa.Caption + ' [*]';
  end else
    Papa.Caption := openFile.Name;
end;

procedure TfrmDisplaySongContent.SaveFile;
begin
  memoCode.Lines.SaveToFile(self.openFilePath);
  markAsChanged(False);
end;

procedure TfrmDisplaySongContent.RenameSongFile(newName: String);
var newFilePath, fileExtension: String;
begin
  FileExtension := ExtractFileExt(openFilePath);
  if RenameFile(OpenFilePath, frmSettings.edtRepoPath.Text + PathDelim + newName + '.' + FileExtension) = False then
  begin
     ShowMessage(strFileCanNotBeRenamed);
     exit;
  end;
  openFile.Name := newName;
  openFile.FileName:=newName + '.' + FileExtension;
end;

end.

