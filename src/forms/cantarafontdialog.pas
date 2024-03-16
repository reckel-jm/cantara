unit cantarafontdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin;

type

  { TCFontDialog }

  TCFontDialog = class(TForm)
    FormatationCheckGroup: TCheckGroup;
    FontSelectionGroupBox: TGroupBox;
    FontSelectionListBox: TListBox;
    FontPropertiesGroupBox: TGroupBox;
    PreviewLabel: TLabel;
    LabelFontSize: TLabel;
    FontSizeEdit: TSpinEdit;
    Splitter1: TSplitter;
    SelectedFont: TFont;
    procedure FontSelectionListBoxClick(Sender: TObject);
    procedure FontSizeEditChange(Sender: TObject);
    procedure FormatationCheckGroupChangeBounds(Sender: TObject);
    procedure FormatationCheckGroupClick(Sender: TObject);
    procedure FormatationCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

    procedure UpdatePreview;
  public

  end;

var
  CFontDialog: TCFontDialog;

implementation

{$R *.lfm}

{ TCFontDialog }

procedure TCFontDialog.FormCreate(Sender: TObject);
begin
  FontSelectionListBox.Items.Assign(Screen.Fonts);
  SelectedFont := TFont.Create;

end;

procedure TCFontDialog.FormDestroy(Sender: TObject);
begin
  SelectedFont.Destroy;
end;

procedure TCFontDialog.FormShow(Sender: TObject);
var i: Integer;
begin
  PreviewLabel.Font.Assign(Self.SelectedFont);

  if PreviewLabel.Font.Name <> '' then
    for i := 0 to FontSelectionListBox.Count-1 do
    begin
      if FontSelectionlistBox.Items[i] = PreviewLabel.Font.Name then
         FontSelectionlistBox.ItemIndex := i;
    end;
  FontSizeEdit.Value:=PreviewLabel.Font.Size;
  FontSizeEdit.Text:=IntToStr(FontSizeEdit.Value);

  {This seams to be a bug in Lazarus -> the style didn't get assigned before}
  PreviewLabel.Font.Style:=Self.SelectedFont.Style;

  FormatationCheckGroup.Checked[0] := Boolean(fsBold in PreviewLabel.Font.Style);
  FormatationCheckGroup.Checked[1] := Boolean(fsItalic in PreviewLabel.Font.Style);
  UpdatePreview;
end;

procedure TCFontDialog.FontSelectionListBoxClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TCFontDialog.FontSizeEditChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TCFontDialog.FormatationCheckGroupChangeBounds(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TCFontDialog.FormatationCheckGroupClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TCFontDialog.FormatationCheckGroupItemClick(Sender: TObject;
  Index: integer);
begin
  UpdatePreview;
end;

procedure TCFontDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Self.SelectedFont.Assign(Self.PreviewLabel.Font);
  Self.SelectedFont.Style:=Self.PreviewLabel.Font.Style;
end;

procedure TCFontDialog.UpdatePreview;
begin
  if FontSelectionListBox.ItemIndex > -1 then
    PreviewLabel.Font.Name:=FontSelectionListBox.Items[FontSelectionListBox.ItemIndex];
  PreviewLabel.Font.Size:=FontSizeEdit.Value;

  PreviewLabel.Font.Style:=[];
  if FormatationCheckGroup.Checked[0] then
     PreviewLabel.Font.Style := PreviewLabel.Font.Style + [fsBold];
  if FormatationCheckGroup.Checked[1] then
     PreviewLabel.Font.Style := PreviewLabel.Font.Style + [fsItalic];
end;

end.

