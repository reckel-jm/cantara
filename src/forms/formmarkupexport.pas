unit FormMarkupExport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ActnList, Lyrics, SynEdit, FGL, LCLType, ResourceHandling, Clipbrd, Markup;

type

  TTemplateDict = specialize TFPGMap<String, TStringList>;

  { TFrmMarkupExport }

  TFrmMarkupExport = class(TForm)
    ClipboardButton: TButton;
    btnSaveToFile: TButton;
    OpenDialog: TOpenDialog;
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
    procedure TemplateComboSelectionChange(Sender: TObject; User: Boolean);
    procedure TemplateEditChange(Sender: TObject);
  private
    TemplateDict: TTemplateDict;
    CurrentFileExtension: String;
  public
    SongList: TSongList;
    procedure ParseTemplate;
  end;

resourcestring
  strTitleOnly = 'Title Only';
  strLoadTemplateFromFile = 'Load Template From File...';

var
  FrmMarkupExport: TFrmMarkupExport;

implementation

{$R *.lfm}

procedure TFrmMarkupExport.ParseTemplate;
var
  MarkupExporter: TMarkupExporter;
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
var
  i: Integer;
begin

  TemplateDict := TTemplateDict.Create;
  TemplateDict.Add(strTitleOnly, LoadResourceFileIntoStringList('MARKUP.TITLEONLY'));
  TemplateDict.Add('Markdown', LoadResourceFileIntoStringList('MARKUP.MARKDOWN'));
  TemplateDict.Add('HTML', LoadResourceFileIntoStringList('MARKUP.HTML'));
  TemplateDict.Add('Telegram', LoadResourceFileIntoStringList('MARKUP.TELEGRAM'));
  TemplateDict.Add('WhatsApp', LoadResourceFileIntoStringList('MARKUP.WHATSAPP'));

  for i := 0 to TemplateDict.Count - 1 do
    TemplateCombo.Items.Add(TemplateDict.Keys[i]);

  TemplateCombo.Items.Add(strLoadTemplateFromFile);
  // load home directory into file/folder dialogs
  OpenDialog.InitialDir := GetUserDir;
  SaveDialog.InitialDir := OpenDialog.InitialDir;
end;

procedure TFrmMarkupExport.btnSaveToFileClick(Sender: TObject);
begin
  SaveDialog.DefaultExt := CurrentFileExtension;
  if SaveDialog.Execute then
    ResultEdit.Lines.SaveToFile(SaveDialog.FileName);

end;

procedure TFrmMarkupExport.ClipboardButtonClick(Sender: TObject);
begin
  try
    Clipboard.AsText := ResultEdit.Lines.Text;
  finally
  end;
end;

procedure TFrmMarkupExport.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TemplateDict.Count - 1 do
    TemplateDict.Data[i].Destroy;
  TemplateDict.Destroy;
end;

procedure TFrmMarkupExport.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_Escape then self.Close;
end;

procedure TFrmMarkupExport.TemplateComboSelectionChange(Sender: TObject; User: Boolean);
begin
  try
    if (TemplateCombo.ItemIndex < TemplateCombo.Count - 1) And
      (TemplateCombo.ItemIndex >= 0) then
      TemplateEdit.Lines.Assign(
        TemplateDict.KeyData[TemplateCombo.Items[TemplateCombo.ItemIndex]])
    else if TemplateCombo.ItemIndex = TemplateCombo.Count - 1 then
    begin
      if (OpenDialog.Execute) And FileExists(OpenDialog.FileName) then
        TemplateEdit.Lines.LoadFromFile(OpenDialog.FileName);
    end;
    ParseTemplate;
  finally
  end;
end;

end.
