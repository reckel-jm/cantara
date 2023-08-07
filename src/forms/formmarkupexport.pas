unit FormMarkupExport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ActnList, Lyrics, SynEdit, FGL, LCLType, ResourceHandling, Clipbrd, Markup;

type

  TTemplateDict = specialize TFPGMap<string, TStringList>;

  { TFrmMarkupExport }

  TFrmMarkupExport = class(TForm)
    ClipboardButton: TButton;
    btnSaveToFile: TButton;
    SaveDialog: TSaveDialog;
    Result: TGroupBox;
    ResultEdit: TSynEdit;
    Splitter1: TSplitter;
    TemplateCombo: TListBox;
    TemplateBox: TGroupBox;
    TemplateEdit: TSynEdit;
    procedure btnSaveToFileClick(Sender: TObject);
    procedure ClipboardButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TemplateComboChange(Sender: TObject);
    procedure TemplateComboSelectionChange(Sender: TObject; User: boolean);
    procedure TemplateEditChange(Sender: TObject);
  private
    TemplateDict: TTemplateDict;
    CurrentFileExtension: String;
  public
    SongList: TSongList;
    procedure ParseTemplate;
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
  CurrentFileExtension := MarkupExporter.FileExtension;
  MarkupExporter.Destroy;
end;

procedure TFrmMarkupExport.TemplateEditChange(Sender: TObject);
begin
  ParseTemplate;
end;

procedure TFrmMarkupExport.FormCreate(Sender: TObject);
var i: Integer;
begin
  TemplateDict := TTemplateDict.create;
  TemplateDict.Add('Markdown', LoadResourceFileIntoStringList('MARKUP.MARKDOWN'));
  TemplateDict.Add('HTML', LoadResourceFileIntoStringList('MARKUP.HTML'));
  TemplateDict.Add('Telegram', LoadResourceFileIntoStringList('MARKUP.TELEGRAM'));
  TemplateDict.Add('WhatsApp', LoadResourceFileIntoStringList('MARKUP.WHATSAPP'));
  for i := 0 to TemplateDict.Count-1 do
    TemplateCombo.Items.Add(TemplateDict.Keys[i]);

  // load home directory into file/folder dialogs
  SaveDialog.InitialDir:=GetUserDir;
end;

procedure TFrmMarkupExport.btnSaveToFileClick(Sender: TObject);
begin
  SaveDialog.DefaultExt:=CurrentFileExtension;
  SaveDialog.InitialDir:=GetUserDir;
  if SaveDialog.Execute then
    ResultEdit.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TFrmMarkupExport.ClipboardButtonClick(Sender: TObject);
begin
  try
    Clipboard.AsText:=ResultEdit.Lines.Text;
  finally
  end;
end;

procedure TFrmMarkupExport.FormDestroy(Sender: TObject);
var i: Integer;
begin
  for i := 0 to TemplateDict.Count-1 do
    TemplateDict.Data[i].Destroy;
  TemplateDict.Destroy;
end;

procedure TFrmMarkupExport.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if key = VK_Escape then self.Close;
end;

procedure TFrmMarkupExport.TemplateComboChange(Sender: TObject);
begin

end;

procedure TFrmMarkupExport.TemplateComboSelectionChange(Sender: TObject;
  User: boolean);
begin
  try
    TemplateEdit.Lines.Assign(TemplateDict.KeyData[TemplateCombo.Items[TemplateCombo.ItemIndex]]);
    ParseTemplate;
  finally
  end;
end;

end.

