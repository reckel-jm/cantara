unit editorwelcome;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, DefaultTranslator, LCLTranslator;

type

  { TfrmEditorWelcome }

  TfrmEditorWelcome = class(TFrame)
    btnOpenDocs: TButton;
    lblDescription: TLabel;
    lblHint: TLabel;
    lblSupport: TLabel;
    lblWelcome: TLabel;
    procedure btnOpenDocsClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TfrmEditorWelcome }

procedure TfrmEditorWelcome.btnOpenDocsClick(Sender: TObject);
begin

end;

end.

