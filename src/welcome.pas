unit welcome;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Settings, info, lclintf, lyrics;

type

  { TfrmWelcome }

  TfrmWelcome = class(TForm)
    BCLabel1: TLabel;
    btnAddExampleSong: TButton;
    btnOpenWebpage: TButton;
    btnOpenGithubRepo: TButton;
    lblAdditionalInformation: TLabel;
    lblSuccessHeadline: TLabel;
    Page2: TPage;
    ProgressBar: TProgressBar;
    btnBack: TButton;
    btnNext: TButton;
    btnSelectSongRepoDir: TButton;
    ControlPanel: TGroupBox;
    Label1: TLabel;
    lblSongAdded: TLabel;
    lblSuccess: TLabel;
    Notebook: TNotebook;
    Page1: TPage;
    procedure BGRAFlashProgressBar1Click(Sender: TObject);
    procedure btnAddExampleSongClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnOpenGithubRepoClick(Sender: TObject);
    procedure btnOpenWebpageClick(Sender: TObject);
    procedure btnSelectSongRepoDirClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Page2BeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure Page2Resize(Sender: TObject);
  private

  public

  end;

const
  AmazingGraceFileName:string = 'Amazing Grace.song';

var
  frmWelcome: TfrmWelcome;

ResourceString
  SongRepoSuccess = 'The selected directory can be successfully used as a song repository.';
  SongRepoDirEmpty = 'However it seems that you have not added any songs yet. Would you like to add the song "Amazing Grace" from John Newton as an example to your song repository? Then click at the button below. Click "Next" to go to the next step.';
  SongRepoNotEmpty = 'The song repository contains already {songcount} songs which you can use with Cantara.';
  SongRepoNotEmptyAddAmazingGrace = 'However if you like, we can add the song "Amazing Grace" as an other example. For that, please press the button below. Else you can click "Next".';
  btnNextNext = 'Next';
  btnNextFinish = 'Finish';

implementation

uses SongSelection;

{$R *.lfm}

{ TfrmWelcome }

procedure TfrmWelcome.BGRAFlashProgressBar1Click(Sender: TObject);
begin

end;

procedure TfrmWelcome.btnAddExampleSongClick(Sender: TObject);
var DummySongFile: TStringList;
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(hinstance, 'AMAZING GRACE', RT_RCDATA);
  DummySongFile := TStringList.Create;
  DummySongFile.LoadFromStream(rs);
  DummySongFile.SaveToFile(frmSettings.edtRepoPath.Text + PathDelim + AmazingGraceFileName);
  FreeAndNil(rs);
  FreeAndNil(DummySongFile);
  btnAddExampleSong.Enabled := False;
  lblSongAdded.Visible := True;
  ProgressBar.Position:=45;
  frmSongs.AskToReloadRepo;
end;

procedure TfrmWelcome.btnBackClick(Sender: TObject);
begin
  if Notebook.PageIndex > 0 then
  begin
    Notebook.PageIndex:=Notebook.PageIndex-1;
    btnNext.Enabled:=True;
    btnNext.Caption:=btnNextNext;
  end;
  if Notebook.PageIndex = 0 then
     btnBack.Enabled:=False;
end;

procedure TfrmWelcome.btnNextClick(Sender: TObject);
begin
  if Notebook.PageIndex < Notebook.PageCount-1 then
  begin
     Notebook.PageIndex := Notebook.PageIndex+1;
     btnBack.Enabled:=True;
  end else if Notebook.PageIndex = NoteBook.PageCount-1 then
     begin
       frmWelcome.Close;
       Exit;
     end;
  if Notebook.PageIndex = NoteBook.PageCount-1 then
     begin
       btnNext.Caption:= btnNextFinish;
       ProgressBar.Position:=100;
     end;
end;

procedure TfrmWelcome.btnOpenGithubRepoClick(Sender: TObject);
begin
  OpenURL(info.GITHUBREPO);
end;

procedure TfrmWelcome.btnOpenWebpageClick(Sender: TObject);
begin
  OpenURL(info.strWebpage);
end;

procedure TfrmWelcome.btnSelectSongRepoDirClick(Sender: TObject);
var RepoContainsAmazingGrace: Boolean;
  repofile: TRepoFile;
begin
  if frmSettings.SelectDirectoryDialog.Execute = False then Exit;
  if DirectoryExists(frmSettings.SelectDirectoryDialog.FileName) then
  begin
    frmSettings.edtRepoPath.Text :=frmSettings.SelectDirectoryDialog.FileName;
    frmSongs.AskToReloadRepo;
    lblSuccess.Caption := SongRepoSuccess;
    btnAddExampleSong.Visible:=False;
    lblSuccess.Visible:=True;
    btnNext.Enabled:=True;
    ProgressBar.Position:=35;
    if length(SongSelection.repo) = 0 then
    begin
       lblSuccess.Caption:=lblSuccess.Caption + LineEnding + SongRepoDirEmpty;
       btnAddExampleSong.Visible := True;
    end else if length(SongSelection.repo) > 0 then
    begin
      lblSuccess.Caption :=
        lblSuccess.Caption + LineEnding +
          StringReplace(SongRepoNotEmpty, '{songcount}', IntToStr(Length(SongSelection.repo)), [rfReplaceAll]);
      RepoContainsAmazingGrace := False;
      for repofile in SongSelection.repo do
        if repofile.FileName = AmazingGraceFileName then
        begin
          RepoContainsAmazingGrace := True;
          Break;
        end;
      if RepoContainsAmazingGrace = False then
      begin
        lblSuccess.Caption:=lblSuccess.Caption + LineEnding + SongRepoNotEmptyAddAmazingGrace;
        btnAddExampleSong.Visible := True;
      end;
    end;
  end;
end;

procedure TfrmWelcome.FormShow(Sender: TObject);
begin
  Notebook.PageIndex:=0;
end;

procedure TfrmWelcome.Page2BeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin

end;

procedure TfrmWelcome.Page2Resize(Sender: TObject);
begin
  btnOpenWebpage.Width := Page2.Width div 2;
end;

end.

