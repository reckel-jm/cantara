unit info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, lclintf, DefaultTranslator;

type

  { TfrmInfo }

  TfrmInfo = class(TForm)
    btnOpenGitRepo: TButton;
    lblInfo: TLabel;
    lblName: TLabel;
    lblVersion: TLabel;
    lblCompDate: TLabel;
    lblAuthor: TLabel;
    procedure btnOpenGitRepoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblInfoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  VERSION:string = '1.3';
  AUTOR:string = 'Jan Martin Reckel';

var
  frmInfo: TfrmInfo;

resourceString
  strFormCaption = 'Über das Programm';
  strProgrammErstellt = 'Programm erstellt';
  strVersion = 'Version';
  strAutor = 'Autor';
  strButtonGitRepo = 'Öffne Git-Repository im Webbrowser...';
  strHinweise = 'Dieses Programm ist unter der GPL3-Lizenz veröffentlicht.'
    + SLineBreak + SLineBreak +
    'Der Quellcode und eine Dokumentation können auf GitHub eingesehen werden. Eine Weiterverwendung und Veränderung ist unter Namensnennung möglich.'
    + SLineBreak + SLineBreak +
    'Der Autor wünscht für die Verwendung Gottes Segen!';

implementation

{$R *.lfm}

{ TfrmInfo }

procedure TfrmInfo.FormCreate(Sender: TObject);
begin
  lblCompDate.Caption := strProgrammErstellt + ': ' + {$I %DATE%} + ' ' + {$I %TIME%};
  lblVersion.Caption := strVersion + ': ' + VERSION;
  lblAuthor.Caption := strAutor + ': ' + AUTOR;
  lblInfo.Caption := strHinweise;
  self.Caption:= strFormCaption;
  btnOpenGitRepo.Caption := strButtonGitRepo;
  btnOpenGitRepo.Top := lblInfo.Top+lblInfo.Height + btnOpenGitRepo.Height;
  frmInfo.Height := btnOpenGitRepo.Top + btnOpenGitRepo.Height;
end;

procedure TfrmInfo.btnOpenGitRepoClick(Sender: TObject);
begin
  OpenURL('https://github.com/reckel-jm/cantara');
end;

procedure TfrmInfo.lblInfoClick(Sender: TObject);
begin

end;

end.

