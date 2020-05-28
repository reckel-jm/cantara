unit info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmInfo }

  TfrmInfo = class(TForm)
    lblInfo: TLabel;
    lblName: TLabel;
    lblVersion: TLabel;
    lblCompDate: TLabel;
    lblAuthor: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  VERSION:string = '1.0';

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

end.

