unit editorwelcome;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, DefaultTranslator,
  LCLTranslator, lclintf, info;

type

  { TfrmEditorWelcome }

  TfrmEditorWelcome = class(TFrame)
    btnOpenDocs: TButton;
    btnClose: TButton;
    btnNewSong: TButton;
    lblDescription: TLabel;
    lblHint: TLabel;
    lblSupport: TLabel;
    lblWelcome: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenDocsClick(Sender: TObject);
    procedure btnNewSongClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

uses
  songeditor;

  { TfrmEditorWelcome }

procedure TfrmEditorWelcome.btnOpenDocsClick(Sender: TObject);
begin
  OpenURL(info.strWebpage);
end;

procedure TfrmEditorWelcome.btnCloseClick(Sender: TObject);
begin
  frmSongEdit.Close;
end;

procedure TfrmEditorWelcome.btnNewSongClick(Sender: TObject);
begin
  frmSongEdit.menuItemNewClick(btnNewSong);
end;

end.
