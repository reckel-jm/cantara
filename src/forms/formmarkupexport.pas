unit FormMarkupExport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Lyrics, SynEdit, Markup;

type

  { TFrmMarkupExport }

  TFrmMarkupExport = class(TForm)
    Button1: TButton;
    btnSaveToFile: TButton;
    Result: TGroupBox;
    ResultEdit: TSynEdit;
    Splitter1: TSplitter;
    TemplateEdit: TSynEdit;
    TemplateBox: TGroupBox;
    procedure ParseTemplate;
    procedure TemplateEditChange(Sender: TObject);
  private

  public
    SongList: TSongList;
  end;



var
  FrmMarkupExport: TFrmMarkupExport;

implementation

{$R *.lfm}

procedure TFrmMarkupExport.ParseTemplate;
var MarkupExporter: TMarkupExporter;
begin
  MarkupExporter := TMarkupExporter.Create(TemplateEdit.Lines.Text, SongList);
  ResultEdit.Lines.Assign(MarkupExporter.ParsingOutput);
  MarkupExporter.Destroy;
end;

procedure TFrmMarkupExport.TemplateEditChange(Sender: TObject);
begin
  ParseTemplate;
end;

end.

