unit info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, lclintf, LCLTranslator, fileinfo, winpeimagereader, elfreader, machoreader;

type

  { TfrmInfo }

  TfrmInfo = class(TForm)
    btnOpenGitRepo: TButton;
    btnOpenWebpage: TButton;
    imgLogo: TImage;
    lblInfo: TLabel;
    lblName: TLabel;
    lblVersion: TLabel;
    lblCompDate: TLabel;
    lblAuthor: TLabel;
    procedure btnOpenGitRepoClick(Sender: TObject);
    procedure btnOpenWebpageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblInfoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  AUTOR:string = 'Jan Martin Reckel';
  GITHUBREPO:string = 'https://www.github.com/reckel-jm/cantara';

var
  frmInfo: TfrmInfo;

resourceString
  strFormCaption = 'About this Program';
  strProgramCompiled = 'Program compiled';
  strVersion = 'Version';
  strAuthor = 'Author';
  strButtonGitRepo = 'Open webpage...';
  strWebpage = 'https://www.cantara.app';

implementation

{$R *.lfm}

{ TfrmInfo }

procedure TfrmInfo.FormCreate(Sender: TObject);
var FileVerInfo: TFileVersionInfo;
begin
  lblCompDate.Caption := strProgramCompiled + ': ' + {$I %DATE%} + ' ' + {$I %TIME%};
  lblAuthor.Caption := strAuthor + ': ' + AUTOR;
  { Fetch the Project Version from the generated Metadata }
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
     FileVerInfo.ReadFileInfo;
     lblVersion.Caption := strVersion + ': ' + FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

procedure TfrmInfo.btnOpenGitRepoClick(Sender: TObject);
begin
  OpenUrl(GITHUBREPO);
end;

procedure TfrmInfo.btnOpenWebpageClick(Sender: TObject);
begin
  OpenURL(strWebpage);
end;

procedure TfrmInfo.lblInfoClick(Sender: TObject);
begin

end;

end.

