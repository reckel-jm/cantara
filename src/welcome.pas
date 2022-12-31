unit welcome;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Settings;

type

  { TfrmWelcome }

  TfrmWelcome = class(TForm)
    BCLabel1: TLabel;
    BGRAFlashProgressBar1: TProgressBar;
    btnBack: TButton;
    btnNext: TButton;
    btnSelectSongRepoDir: TButton;
    ControlPanel: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblSuccess: TLabel;
    Notebook: TNotebook;
    Page1: TPage;
    procedure BGRAFlashProgressBar1Click(Sender: TObject);
    procedure btnSelectSongRepoDirClick(Sender: TObject);
  private

  public

  end;

var
  frmWelcome: TfrmWelcome;

implementation

uses SongSelection;

{$R *.lfm}

{ TfrmWelcome }

procedure TfrmWelcome.BGRAFlashProgressBar1Click(Sender: TObject);
begin

end;

procedure TfrmWelcome.btnSelectSongRepoDirClick(Sender: TObject);
begin
  frmSettings.btnSelectDirClick(btnSelectSongRepoDir);
  if DirectoryExists(frmSettings.edtRepoPath.Text) then
  begin
    frmSongs.AskToReloadRepo;
    lblSuccess.Visible:=True;
    btnNext.Enabled:=True;
  end;
end;

end.

