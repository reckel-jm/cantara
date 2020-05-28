unit info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, lclintf;

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
  VERSION:string = '1.1';

var
  frmInfo: TfrmInfo;

implementation

{$R *.lfm}

{ TfrmInfo }

procedure TfrmInfo.FormCreate(Sender: TObject);
begin
  lblCompDate.Caption := 'Programm erstellt: ' + {$I %DATE%} + ' ' + {$I %TIME%};
  lblVersion.Caption := 'Version ' + VERSION;
end;

procedure TfrmInfo.btnOpenGitRepoClick(Sender: TObject);
begin
  OpenURL('https://github.com/reckel-jm/cantara');
end;

procedure TfrmInfo.lblInfoClick(Sender: TObject);
begin

end;

end.

