unit FormSongStyle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtDlgs, StdCtrls, ExtCtrls,
  Spin, Buttons, PresentationModels;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlBgColorClick(Sender: TObject);
    procedure pnlTextColorClick(Sender: TObject);
  private
    FCurrentFont: TFont;
    FResetToDefault: Boolean;
    { Stores all settings from LoadFromStyle; ExportStyle starts from this
      and overrides only the fields the dialog exposes. }
    FBaseStyle: TPresentationStyleSettings;
    FBaseStyleLoaded: Boolean;
    procedure UpdateFontPreview;
    procedure UpdateControlsEnabled;
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
begin
  FCurrentFont := TFont.Create;
  FResetToDefault := False;
  FBaseStyleLoaded := False;
  cboHAlign.Items.Add('Left');
  cboHAlign.Items.Add('Center');
  cboHAlign.Items.Add('Right');
  cboHAlign.ItemIndex := 1;
end;

procedure TfrmSongStyle.FormDestroy(Sender: TObject);
begin
  FCurrentFont.Free;
  if FBaseStyleLoaded then
    DestroyPresentationStyleSettings(FBaseStyle);
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

procedure TfrmSongStyle.cbUseCustomStyleChange(Sender: TObject);
begin
  UpdateControlsEnabled;
end;

procedure TfrmSongStyle.btnFontClick(Sender: TObject);
begin
  dlgFont.Font.Assign(FCurrentFont);
  if dlgFont.Execute then
  begin
    FCurrentFont.Assign(dlgFont.Font);
    UpdateFontPreview;
  end;
end;

procedure TfrmSongStyle.pnlTextColorClick(Sender: TObject);
begin
  dlgColor.Color := pnlTextColor.Color;
  if dlgColor.Execute then
    pnlTextColor.Color := dlgColor.Color;
end;

procedure TfrmSongStyle.pnlBgColorClick(Sender: TObject);
begin
  dlgColor.Color := pnlBgColor.Color;
  if dlgColor.Execute then
    pnlBgColor.Color := dlgColor.Color;
end;

procedure TfrmSongStyle.cbShowBgImageChange(Sender: TObject);
begin
  edtBgImagePath.Enabled := cbUseCustomStyle.Checked and cbShowBgImage.Checked;
  btnBrowseBgImage.Enabled := cbUseCustomStyle.Checked and cbShowBgImage.Checked;
  spTransparency.Enabled := cbUseCustomStyle.Checked and cbShowBgImage.Checked;
end;

procedure TfrmSongStyle.btnBrowseBgImageClick(Sender: TObject);
begin
  if dlgOpenPicture.Execute then
    edtBgImagePath.Text := dlgOpenPicture.FileName;
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
  FBaseStyle.Font.Assign(AStyle.Font);
  FBaseStyleLoaded := True;

  FCurrentFont.Assign(AStyle.Font);
  UpdateFontPreview;

  pnlTextColor.Color := AStyle.TextColor;
  pnlBgColor.Color := AStyle.BackgroundColor;

  cbShowBgImage.Checked := AStyle.ShowBackgroundImage;
  edtBgImagePath.Text := AStyle.BackgroundImageFilePath;
  spTransparency.Value := AStyle.Transparency;

  case AStyle.HorizontalAlign of
    Align_Left:   cboHAlign.ItemIndex := 0;
    Align_Center: cboHAlign.ItemIndex := 1;
    Align_Right:  cboHAlign.ItemIndex := 2;
  end;

  UpdateControlsEnabled;
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
  Result.Transparency := spTransparency.Value;
  case cboHAlign.ItemIndex of
    0: Result.HorizontalAlign := Align_Left;
    1: Result.HorizontalAlign := Align_Center;
    2: Result.HorizontalAlign := Align_Right;
  else
    Result.HorizontalAlign := Align_Center;
  end;
end;

end.
