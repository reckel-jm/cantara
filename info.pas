unit info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, lclintf, LCLTranslator;

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
  VERSION:string = '2.3 Beta';
  AUTOR:string = 'Jan Martin Reckel';

var
  frmInfo: TfrmInfo;

resourceString
  strFormCaption = 'About this Program';
  strProgrammErstellt = 'Program compiled';
  strVersion = 'Version';
  strAutor = 'Author';
  strButtonGitRepo = 'Open webpage...';
  strHinweise = 'This program is published under the GPL3 licence.'
    + sLineBreak + sLineBreak +
    'The Source Code and the documentation can be looked up at Github. In this way, it is also possible to get in contact.'
    + sLineBreak + sLineBreak +
    'The author wishes God''s blessings and is looking forward for feedback.';
  strWebpage = 'https://www.cantara.app';

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
  btnOpenGitRepo.Top := lblInfo.Top + lblInfo.Height + 100;
  frmInfo.Height := btnOpenGitRepo.Top + btnOpenGitRepo.Height;
end;

procedure TfrmInfo.btnOpenGitRepoClick(Sender: TObject);
begin
  OpenURL(strWebpage);
end;

procedure TfrmInfo.lblInfoClick(Sender: TObject);
begin

end;

end.

