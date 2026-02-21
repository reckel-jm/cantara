unit FormSongStyle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtDlgs, StdCtrls, ExtCtrls,
  Spin, Buttons, PresentationModels,
  Slides, PresentationCanvas, ResourceHandling, Lyrics;

type

  { TfrmSongStyle }

  TfrmSongStyle = class(TForm)
    btnCancel: TBitBtn;
    btnFont: TButton;
    btnOK: TBitBtn;
    btnReset: TButton;
    btnBrowseBgImage: TButton;
    cbShowBgImage: TCheckBox;
    cbUseCustomStyle: TCheckBox;
    cboHAlign: TComboBox;
    dlgColor: TColorDialog;
    dlgFont: TFontDialog;
    dlgOpenPicture: TOpenPictureDialog;
    edtBgImagePath: TEdit;
    gbBackground: TGroupBox;
    gbFont: TGroupBox;
    gbAlign: TGroupBox;
    gbPreview: TGroupBox;
    imgPreview: TImage;
    lblAlignLabel: TLabel;
    lblBgColorLabel: TLabel;
    lblBgImageLabel: TLabel;
    lblFontPreview: TLabel;
    lblTextColorLabel: TLabel;
    lblTransparency: TLabel;
    pnlBgColor: TPanel;
    pnlTextColor: TPanel;
    spTransparency: TSpinEdit;
    procedure btnBrowseBgImageClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cbShowBgImageChange(Sender: TObject);
    procedure cbUseCustomStyleChange(Sender: TObject);
    procedure cboHAlignChange(Sender: TObject);
    procedure edtBgImagePathChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblTextColorLabelClick(Sender: TObject);
    procedure pnlBgColorClick(Sender: TObject);
    procedure pnlTextColorClick(Sender: TObject);
    procedure spTransparencyChange(Sender: TObject);
  private
    FCurrentFont: TFont;
    FResetToDefault: Boolean;
    { Stores all settings from LoadFromStyle; ExportStyle starts from this
      and overrides only the fields the dialog exposes. }
    FBaseStyle: TPresentationStyleSettings;
    FBaseStyleLoaded: Boolean;
    FPreviewCanvas: TPresentationCanvasHandler;
    FExampleSong: TSong;
    FPreviewSlideList: TSlideList;
    FLastPreviewBgPath: String;
    procedure UpdateFontPreview;
    procedure UpdateControlsEnabled;
    procedure RefreshPreview;
  public
    procedure LoadFromStyle(const AStyle: TPresentationStyleSettings;
      AHasCustom: Boolean);
    function ExportStyle: TPresentationStyleSettings;
    property ResetToDefault: Boolean read FResetToDefault;
  end;

var
  frmSongStyle: TfrmSongStyle;

implementation

{$R *.lfm}

{ TfrmSongStyle }

procedure TfrmSongStyle.FormCreate(Sender: TObject);
var
  DummySongFile: TStringList;
  DefaultSlideSettings: TSlideSettings;
  SlideCounter: Integer;
begin
  FCurrentFont := TFont.Create;
  FResetToDefault := False;
  FBaseStyleLoaded := False;
  FLastPreviewBgPath := '';
  cboHAlign.Items.Add('Left');
  cboHAlign.Items.Add('Center');
  cboHAlign.Items.Add('Right');
  cboHAlign.ItemIndex := 1;

  // Set up the preview canvas and an example song (same resource used by Settings)
  FPreviewCanvas := TPresentationCanvasHandler.Create;
  DummySongFile := LoadResourceFileIntoStringList('AMAZING GRACE');
  FExampleSong := TSong.Create;
  FExampleSong.MetaDict.Add('title', 'Amazing Grace');
  FExampleSong.importSongFromStringList(DummySongFile);
  DummySongFile.Destroy;
  FillChar(DefaultSlideSettings, SizeOf(DefaultSlideSettings), 0);
  SlideCounter := 0;
  FPreviewSlideList := CreatePresentationDataFromSong(
    FExampleSong, DefaultSlideSettings, SlideCounter);
end;

procedure TfrmSongStyle.FormDestroy(Sender: TObject);
begin
  FCurrentFont.Free;
  if FBaseStyleLoaded then
    DestroyPresentationStyleSettings(FBaseStyle);
  FPreviewSlideList.Free;
  FExampleSong.Free;
  FPreviewCanvas.Free;
end;

procedure TfrmSongStyle.lblTextColorLabelClick(Sender: TObject);
begin

end;

procedure TfrmSongStyle.UpdateFontPreview;
begin
  lblFontPreview.Caption := FCurrentFont.Name + ', ' + IntToStr(Abs(FCurrentFont.Size)) + ' pt';
  lblFontPreview.Font.Assign(FCurrentFont);
  lblFontPreview.Font.Size := 9; // keep preview readable
end;

procedure TfrmSongStyle.UpdateControlsEnabled;
var
  UseCustom: Boolean;
begin
  UseCustom := cbUseCustomStyle.Checked;
  gbFont.Enabled := UseCustom;
  gbBackground.Enabled := UseCustom;
  gbAlign.Enabled := UseCustom;
  btnFont.Enabled := UseCustom;
  pnlTextColor.Enabled := UseCustom;
  cbShowBgImage.Enabled := UseCustom;
  edtBgImagePath.Enabled := UseCustom and cbShowBgImage.Checked;
  btnBrowseBgImage.Enabled := UseCustom and cbShowBgImage.Checked;
  spTransparency.Enabled := UseCustom and cbShowBgImage.Checked;
  pnlBgColor.Enabled := UseCustom;
  cboHAlign.Enabled := UseCustom;
end;

procedure TfrmSongStyle.RefreshPreview;
var
  Style: TPresentationStyleSettings;
begin
  if (not FBaseStyleLoaded) or (FPreviewSlideList.Count = 0) then Exit;
  if imgPreview.Width <= 0 then Exit;
  Style := ExportStyle;
  try
    FPreviewCanvas.PresentationStyleSettings := Style;
    // Render at full screen resolution (same as the presentation), then let
    // imgPreview scale it down proportionally; this keeps font sizes correct.
    FPreviewCanvas.Width  := Screen.Width;
    FPreviewCanvas.Height := Screen.Height;
    if Style.ShowBackgroundImage and FileExists(Style.BackgroundImageFilePath) then
    begin
      if Style.BackgroundImageFilePath <> FLastPreviewBgPath then
      begin
        FPreviewCanvas.LoadBackgroundBitmap;
        FLastPreviewBgPath := Style.BackgroundImageFilePath;
      end;
    end
    else
      FLastPreviewBgPath := '';
    FPreviewCanvas.ResizeBackgroundBitmap;
    imgPreview.Picture.Assign(FPreviewCanvas.PaintSlide(FPreviewSlideList[0]));
  finally
    DestroyPresentationStyleSettings(Style);
  end;
end;

procedure TfrmSongStyle.cbUseCustomStyleChange(Sender: TObject);
begin
  UpdateControlsEnabled;
  RefreshPreview;
end;

procedure TfrmSongStyle.btnFontClick(Sender: TObject);
begin
  dlgFont.Font.Assign(FCurrentFont);
  if dlgFont.Execute then
  begin
    FCurrentFont.Assign(dlgFont.Font);
    UpdateFontPreview;
    RefreshPreview;
  end;
end;

procedure TfrmSongStyle.pnlTextColorClick(Sender: TObject);
begin
  dlgColor.Color := pnlTextColor.Color;
  if dlgColor.Execute then
  begin
    pnlTextColor.Color := dlgColor.Color;
    RefreshPreview;
  end;
end;

procedure TfrmSongStyle.pnlBgColorClick(Sender: TObject);
begin
  dlgColor.Color := pnlBgColor.Color;
  if dlgColor.Execute then
  begin
    pnlBgColor.Color := dlgColor.Color;
    RefreshPreview;
  end;
end;

procedure TfrmSongStyle.cbShowBgImageChange(Sender: TObject);
begin
  edtBgImagePath.Enabled := cbUseCustomStyle.Checked and cbShowBgImage.Checked;
  btnBrowseBgImage.Enabled := cbUseCustomStyle.Checked and cbShowBgImage.Checked;
  spTransparency.Enabled := cbUseCustomStyle.Checked and cbShowBgImage.Checked;
  RefreshPreview;
end;

procedure TfrmSongStyle.btnBrowseBgImageClick(Sender: TObject);
begin
  if dlgOpenPicture.Execute then
  begin
    edtBgImagePath.Text := dlgOpenPicture.FileName;
    RefreshPreview;
  end;
end;

procedure TfrmSongStyle.cboHAlignChange(Sender: TObject);
begin
  RefreshPreview;
end;

procedure TfrmSongStyle.edtBgImagePathChange(Sender: TObject);
begin
  RefreshPreview;
end;

procedure TfrmSongStyle.spTransparencyChange(Sender: TObject);
begin
  RefreshPreview;
end;

procedure TfrmSongStyle.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmSongStyle.btnResetClick(Sender: TObject);
begin
  FResetToDefault := True;
  ModalResult := mrOK;
end;

procedure TfrmSongStyle.LoadFromStyle(const AStyle: TPresentationStyleSettings;
  AHasCustom: Boolean);
begin
  FResetToDefault := False;
  cbUseCustomStyle.Checked := AHasCustom;

  // Store a deep copy of the full style so ExportStyle can preserve non-displayed fields
  if FBaseStyleLoaded then
    DestroyPresentationStyleSettings(FBaseStyle);
  FBaseStyle := AStyle;
  FBaseStyle.Font := TFont.Create;
  if Assigned(AStyle.Font) then
    FBaseStyle.Font.Assign(AStyle.Font);
  FBaseStyleLoaded := True;

  if Assigned(AStyle.Font) then
    FCurrentFont.Assign(AStyle.Font);
  UpdateFontPreview;

  pnlTextColor.Color := AStyle.TextColor;
  pnlBgColor.Color := AStyle.BackgroundColor;

  cbShowBgImage.Checked := AStyle.ShowBackgroundImage;
  edtBgImagePath.Text := AStyle.BackgroundImageFilePath;
  spTransparency.Value := Abs(AStyle.Transparency);

  case AStyle.HorizontalAlign of
    Align_Left:   cboHAlign.ItemIndex := 0;
    Align_Center: cboHAlign.ItemIndex := 1;
    Align_Right:  cboHAlign.ItemIndex := 2;
  end;

  UpdateControlsEnabled;
  FLastPreviewBgPath := ''; // force background reload for the incoming style
  RefreshPreview;
end;

function TfrmSongStyle.ExportStyle: TPresentationStyleSettings;
begin
  // Start from the full base style to preserve VerticalAlign, Padding, etc.
  Result := FBaseStyle;
  // Create a new Font (caller must DestroyPresentationStyleSettings the result)
  Result.Font := TFont.Create;
  Result.Font.Assign(FCurrentFont);
  // Override the fields this dialog exposes
  Result.TextColor := pnlTextColor.Color;
  Result.BackgroundColor := pnlBgColor.Color;
  Result.ShowBackgroundImage := cbShowBgImage.Checked;
  Result.BackgroundImageFilePath := edtBgImagePath.Text;
  Result.Transparency := -spTransparency.Value;
  case cboHAlign.ItemIndex of
    0: Result.HorizontalAlign := Align_Left;
    1: Result.HorizontalAlign := Align_Center;
    2: Result.HorizontalAlign := Align_Right;
  else
    Result.HorizontalAlign := Align_Center;
  end;
end;

end.
