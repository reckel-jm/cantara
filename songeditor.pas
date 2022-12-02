unit songeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, displaySongContent, lyrics;

type
  { TfrmSongEdit }

  TfrmSongEdit = class(TForm)
    btnOpenDocs: TButton;
    frmDisplaySong: TfrmDisplaySongContent;
    lblDescription: TLabel;
    lblWelcome: TLabel;
    lsSongs: TListBox;
    notebook: TNotebook;
    pageEditSong: TPage;
    pageWelcome: TPage;
    splitter: TSplitter;
    procedure edtSongNameChange(Sender: TObject);
    procedure edtSongNameEditingDone(Sender: TObject);
    procedure edtSongNameExit(Sender: TObject);
    procedure frmDisplayClick(Sender: TObject);
    procedure lblSongNameClick(Sender: TObject);
    procedure lsSongsClick(Sender: TObject);
    procedure notebookChangeBounds(Sender: TObject);
    procedure pageEditBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure pageWelcomeBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
  private
    repo: TRepoArray; // Load the Repo for editing it later
    procedure loadRepoIntoSongListbox;
    procedure LoadSelectedSongContent;
    procedure loadFileIntoFrame(song: TRepoFile);
  public
    procedure loadRepo(SongRepositoryArray: TRepoArray);
  end;

var
  frmSongEdit: TfrmSongEdit;

implementation
procedure TfrmSongEdit.loadRepo(SongRepositoryArray: TRepoArray);
begin
  self.repo:=SongRepositoryArray;
  loadRepoIntoSongListbox;
end;

procedure TfrmSongEdit.lsSongsClick(Sender: TObject);
begin
  LoadSelectedSongContent;
end;

procedure TfrmSongEdit.notebookChangeBounds(Sender: TObject);
begin

end;

procedure TfrmSongEdit.pageEditBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin

end;

procedure TfrmSongEdit.pageWelcomeBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin

end;

procedure TfrmSongEdit.frmDisplayClick(Sender: TObject);
begin

end;

procedure TfrmSongEdit.lblSongNameClick(Sender: TObject);
begin

end;

procedure TfrmSongEdit.edtSongNameExit(Sender: TObject);
begin

end;

procedure TfrmSongEdit.edtSongNameEditingDone(Sender: TObject);
begin

end;

procedure TfrmSongEdit.edtSongNameChange(Sender: TObject);
begin

end;

procedure TfrmSongEdit.loadRepoIntoSongListbox;
var i: integer;
begin
  lsSongs.Items.Clear;
  for i := 0 to length(self.repo)-1 do
    lsSongs.Items.AddObject(self.repo[i].filePath, self.repo[i]);
end;

procedure TfrmSongEdit.LoadSelectedSongContent;
var selectedName: String;
  i: Integer;
begin
  selectedName := lsSongs.Items[lsSongs.ItemIndex];
  loadFileIntoFrame(lsSongs.Items.Objects[lsSongs.ItemIndex] as TRepoFile);
end;

procedure TfrmSongEdit.loadFileIntoFrame(song: TRepoFile);
begin
  notebook.PageIndex:=1;
  frmDisplaySong.loadFile(song);
end;

{$R *.lfm}

end.

