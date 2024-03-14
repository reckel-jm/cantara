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
    procedure FontSelectionListBoxClick(Sender: TObject);
    procedure FontSizeEditChange(Sender: TObject);
    procedure FormatationCheckGroupChangeBounds(Sender: TObject);
    procedure FormatationCheckGroupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
end;

procedure TCFontDialog.FormShow(Sender: TObject);
var i: Integer;
begin
  if PreviewLabel.Font.Name <> '' then
    for i := 0 to FontSelectionListBox.Count-1 do
    begin
      if FontSelectionlistBox.Items[i] = PreviewLabel.Font.Name then
         FontSelectionlistBox.ItemIndex := i;
    end;
  FontSizeEdit.Value:=PreviewLabel.Font.Size;
  FontSizeEdit.Text:=IntToStr(FontSizeEdit.Value);
  FormatationCheckGroup.Checked[0] := (fsBold in PreviewLabel.Font.Style);
  FormatationCheckGroup.Checked[1] := (fsItalic in PreviewLabel.Font.Style);
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

