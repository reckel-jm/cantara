unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, Spin, INIfiles, LCLTranslator, DefaultTranslator, ExtDlgs,
  LCLINTF, LCLType, ExtCtrls, ActnList, Arrow, Present, Lyrics, Slides,
  ResourceHandling, PresentationCanvas, settingspadding;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnBackgroundColor: TButton;
    btnClose: TButton;
    btnFontSizeManually: TButton;
    btnTextColor: TButton;
    btnBackgroundImage: TButton;
    btnDetails: TButton;
    cbMetaDataFirstSlide: TCheckBox;
    cbMetaTitleSlide: TCheckBox;
    cbShowBackgroundImage: TCheckBox;
    cbMetaDataLastSlide: TCheckBox;
    cbSpoiler: TCheckBox;
    cbLyricsToClipboard: TCheckBox;
    comboVertical: TComboBox;
    comboHorizontal: TComboBox;
    FontDialog: TFontDialog;
    gbPresentation: TGroupBox;
    ImagePresentationPreview: TImage;
    lblAlignment: TLabel;
    lblWrapAfter: TLabel;
    lblImageExplainer: TLabel;
    lblImageBrightness: TLabel;
    lblMeta: TLabel;
    lblMetaContent: TLabel;
    memoMetaData: TMemo;
    BgPictureDialog: TOpenPictureDialog;
    sbImageBrightness: TScrollBar;
    seWrapLines: TSpinEdit;
    textColorDialog: TColorDialog;
    btnSelectDir: TButton;
    cbEmptyFrame: TCheckBox;
    bgColorDialog: TColorDialog;
    edtRepoPath: TEdit;
    labelSongDir: TLabel;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    UpdatePreviewTimer: TTimer;
    procedure btnBackgroundImageClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDetailsClick(Sender: TObject);
    procedure btnFontSizeManuallyClick(Sender: TObject);
    procedure btnSelectDirClick(Sender: TObject);
    procedure btnBackgroundColorClick(Sender: TObject);
    procedure btnTextColorClick(Sender: TObject);
    procedure cbAutoWordWrapChange(Sender: TObject);
    procedure cbLyricsToClipboardChange(Sender: TObject);
    procedure cbMetaDataFirstSlideChange(Sender: TObject);
    procedure cbMetaDataLastSlideChange(Sender: TObject);
    procedure cbMetaTitleSlideChange(Sender: TObject);
    procedure cbShowBackgroundImageChange(Sender: TObject);
    procedure edtRepoPathChange(Sender: TObject);
    procedure edtRepoPathEditingDone(Sender: TObject);
    procedure edtRepoPathExit(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gbPresentationClick(Sender: TObject);
    procedure ImagePresentationPreviewClick(Sender: TObject);
    procedure ImagePresentationPreviewDblClick(Sender: TObject);
    procedure loadSettings();
    procedure memoMetaDataEditingDone(Sender: TObject);
    procedure sbImageBrightnessChange(Sender: TObject);
    procedure seWrapLinesChange(Sender: TObject);
    procedure UpdatePreviewTimerTimer(Sender: TObject);
  private
    { private declarations }
    PresentationPreviewCanvas: TPresentationCanvasHandler;
    SlideList: TSlideList;
    slidelistcur: Integer;
    procedure LoadPreviewImage;
    procedure ReloadSlideAndPresentationCanvas;
  public
    { public declarations }
    changedBackground: Boolean;
    {Exports the slide settings as TSlideSettings record }
    function ExportSlideSettings(): TSlideSettings;
    function ExportPresentationStyleSettings: TPresentationStyleSettings;
  end;

var
  frmSettings: TfrmSettings;
  settingsFile: TINIFile;

ResourceString
  strTransparency = 'Increase transparency by ';
  strBrightness = 'Increase brightness by ';
  strPictureOriginalState = 'Picture is shown as it is';
  strErrorCaption = 'Error';
  strValidSongRepository = 'Please choose a valid folder for the song repository!';

implementation

Uses
  SongSelection;

{$R *.lfm}

{ TfrmSettings }

function getRepoDir(): string;
begin
  if DirectoryExists(getUserdir()+'Liederverzeichnis') then result := getUserdir()+'Liederverzeichnis'
  else if DirectoryExists(getUserdir()+'Dokumente'+ PathDelim + 'Liederverzeichnis') then result := getUserdir()+'Dokumente'+ PathDelim + 'Liederverzeichnis'
  else if DirectoryExists(ExtractFilePath(Application.ExeName) + 'Liederverzeichnis') then result := ExtractFilePath(Application.ExeName) + 'Liederverzeichnis'
  else result := '';
end;

function StyleToStr(Style: TFontStyles): String;
begin
  SetLength(Result, 4);
  {T = true, S = false 83 is ordinal value of S, if true then S + 1 (84) = T}
  Result[1] := Char(Integer(fsBold In Style) + 83);
  Result[2] := Char(Integer(fsItalic In Style) + 83);
  Result[3] := Char(Integer(fsUnderline In Style) + 83);
  Result[4] := Char(Integer(fsStrikeOut In Style) + 83);
  { replace all S to F's }
  Result := StringReplace(Result, 'S', 'F', [rfReplaceAll]);
end;

function StrToStyle(Str: String): TFontStyles;
begin
  Result := [];
  {T = true, S = false}
  if Str[1] = 'T' then
    Include(Result, fsBold);
  if Str[2] = 'T' then
    Include(Result, fsItalic);
  if Str[3] = 'T' then
    Include(Result, fsUnderLine);
  if Str[4] = 'T' then
    Include(Result, fsStrikeOut);
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  changedBackground := False;
  PresentationPreviewCanvas := TPresentationCanvasHandler.Create;
  slidelistcur := 0;
end;

procedure TfrmSettings.FormDestroy(Sender: TObject);
begin
  PresentationPreviewCanvas.Destroy;
end;

procedure TfrmSettings.FormHide(Sender: TObject);
begin
  UpdatePreviewTimer.Enabled:=False;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  sbImageBrightnessChange(frmSettings);
  changedBackground := False;
  ReloadSlideAndPresentationCanvas;
  UpdatePreviewTimer.Enabled:=True;
end;

procedure TfrmSettings.gbPresentationClick(Sender: TObject);
begin
  PresentationPreviewCanvas := TPresentationCanvasHandler.Create;
end;

procedure TfrmSettings.ImagePresentationPreviewClick(Sender: TObject);
begin
  if slideListCur < SlideList.Count-1 then slidelistcur += 1
    else slideListCur := 0;
  LoadPreviewImage;
end;

procedure TfrmSettings.ImagePresentationPreviewDblClick(Sender: TObject);
begin

end;

procedure TfrmSettings.btnSelectDirClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    edtRepoPath.Text:=SelectDirectoryDialog.FileName;
end;

procedure TfrmSettings.btnFontSizeManuallyClick(Sender: TObject);
begin
  FontDialog.Execute;
end;

procedure TfrmSettings.btnCloseClick(Sender: TObject);
begin

end;

procedure TfrmSettings.btnDetailsClick(Sender: TObject);
begin
  FormPadding.ShowModal;
end;

procedure TfrmSettings.btnBackgroundImageClick(Sender: TObject);
begin
  BgPictureDialog.Execute;
  changedBackground := True;
end;

procedure TfrmSettings.btnBackgroundColorClick(Sender: TObject);
begin
  bgColorDialog.Execute;
end;

procedure TfrmSettings.btnTextColorClick(Sender: TObject);
begin
  textColorDialog.Execute;
end;

procedure TfrmSettings.cbAutoWordWrapChange(Sender: TObject);
begin

end;

procedure TfrmSettings.cbLyricsToClipboardChange(Sender: TObject);
begin

end;

procedure TfrmSettings.cbMetaDataFirstSlideChange(Sender: TObject);
begin
  ReloadSlideAndPresentationCanvas;
end;

procedure TfrmSettings.cbMetaDataLastSlideChange(Sender: TObject);
begin
  ReloadSlideAndPresentationCanvas;
end;

procedure TfrmSettings.cbMetaTitleSlideChange(Sender: TObject);
begin
  ReloadSlideAndPresentationCanvas;
end;

procedure TfrmSettings.cbShowBackgroundImageChange(Sender: TObject);
begin
  btnBackgroundImage.Enabled:=cbShowBackgroundImage.Checked;
  changedBackground := True;
end;

procedure TfrmSettings.edtRepoPathChange(Sender: TObject);
begin

end;

procedure TfrmSettings.edtRepoPathEditingDone(Sender: TObject);
begin
  if (edtRepoPath.Text <> '') and (edtRepoPath.Text[length(edtRepoPath.Text)] = PathDelim) then
    edtRepoPath.Text := copy(edtRepoPath.Text,1,length(edtRepoPath.Text)-1);
end;

procedure TfrmSettings.edtRepoPathExit(Sender: TObject);
begin

end;

procedure TfrmSettings.FormClose(Sender: TObject);
begin
  // Prevent exceptions from happening
  if not FileExists(BgPictureDialog.FileName) then
  begin
    cbShowBackgroundImage.Checked := False;
    cbShowBackgroundImageChange(frmSettings);
  end;
  //if changedBackground then frmPresent.loadSettings;
  frmSongs.edtSearch.Text := '';
end;

procedure TfrmSettings.loadSettings();
var str: String;
  Padding: TPadding;
begin
  edtRepoPath.Text := settingsFile.ReadString('Config', 'Repo-Path', getRepoDir());
  cbEmptyFrame.Checked := settingsFile.ReadBool('Config', 'empty-Frame', True);
  cbLyricsToClipboard.Checked := settingsFile.ReadBool('Config', 'copy-lyrics-to-clipboard', True);
  textColorDialog.Color := StringToColor(settingsFile.ReadString('Config', 'Text-Color', 'clWhite'));
  bgColorDialog.Color := StringToColor(settingsFile.ReadString('Config', 'Background-Color', 'clBlack'));
  cbSpoiler.Checked:=settingsFile.ReadBool('Config', 'Spoiler', True);
  cbMetaTitleSlide.Checked := settingsFile.ReadBool('Config', 'TitleSlide', False);
  cbMetaDataFirstSlide.Checked := settingsFile.ReadBool('Config', 'MetaDataFirstSlide', False);
  cbMetaDataLastSlide.Checked := settingsFile.ReadBool('Config', 'MetaDataLastSlide', False);
  str := settingsFile.ReadString('Config','MetaDataSyntax', '');
  memoMetaData.lines.Text := StringReplace(str, '</br>', LineEnding, [rfReplaceAll]);
  FontDialog.Font.Name:=settingsFile.ReadString('Config', 'Font-Name', 'default');
  FontDialog.Font.Style := StrToStyle(settingsFile.ReadString('Config', 'Font-Style', 'ssss'));
  FontDialog.Font.Size:= settingsFile.ReadInteger('Config', 'Font-Size', 42);
  //edtLineDistance.Value:=settingsFile.ReadFloat('Config', 'Line-Distance', 1);
  cbShowBackgroundImage.Checked := settingsFile.ReadBool('Config', 'BackgroundPicture', false);
  cbShowBackgroundImageChange(frmSettings);
  BgPictureDialog.FileName := settingsFile.ReadString('Config', 'BackgroundPicture-Path', '');
  sbImageBrightness.Position:=settingsFile.ReadInteger('Config', 'ImageBrightness', 0);
  seWrapLines.Value:=settingsFile.ReadInteger('Config', 'AutoWrap', 8);
  comboHorizontal.ItemIndex := settingsFile.ReadInteger('Config', 'AlignHorizontal', Ord(Align_Center)); // default is centering
  comboVertical.ItemIndex := settingsFile.ReadInteger('Config', 'AlignVertical', Ord(tlCenter)); // default is in the middle

  Padding.Left:=settingsFile.ReadInteger('Config', 'Padding-Left', PresentationCanvas.PADDING);
  Padding.Right:=settingsFile.ReadInteger('Config', 'Padding-Right', PresentationCanvas.PADDING);
  Padding.Top:=settingsFile.ReadInteger('Config', 'Padding-Top', PresentationCanvas.PADDING);
  Padding.Bottom:=settingsFile.ReadInteger('Config', 'Padding-Bottom', PresentationCanvas.PADDING);
  FormPadding.frmSettingsDetailed.ImportPadding(Padding);

  sbImageBrightnessChange(frmPresent);
end;

procedure TfrmSettings.memoMetaDataEditingDone(Sender: TObject);
begin
  ReloadSlideAndPresentationCanvas;
end;

procedure TfrmSettings.sbImageBrightnessChange(Sender: TObject);
begin
  if sbImageBrightness.Position < 0 then
     lblImageExplainer.Caption:=strTransparency + ' ' + IntToStr(Abs(sbImageBrightness.Position))+'%'
  else if sbImageBrightness.Position = 0 then
     lblImageExplainer.Caption := strPictureOriginalState
  else
     lblImageExplainer.Caption:=strBrightness + ' ' + IntToStr(sbImageBrightness.Position) + '%';
  changedBackground := True;
end;

procedure TfrmSettings.seWrapLinesChange(Sender: TObject);
begin
  if seWrapLines.Value < 0 then seWrapLines.Value:=0;
  try
     ReloadSlideAndPresentationCanvas;
  finally
  end;
end;

procedure TfrmSettings.UpdatePreviewTimerTimer(Sender: TObject);
begin
  LoadPreviewImage;
end;

procedure TfrmSettings.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var str: String;
  Padding: TPadding;
begin
  CanClose := False;
  if DirectoryExists(edtRepoPath.Text) = False then Application.MessageBox(PChar(strValidSongRepository), PChar(strErrorCaption))
  else
  begin
    settingsFile.WriteString('Config', 'Repo-Path', edtRepoPath.Text);
    settingsFile.WriteBool('Config', 'empty-Frame', cbEmptyFrame.Checked);
    settingsFile.WriteString('Config', 'Text-Color', ColorToString(textColorDialog.Color));
    settingsFile.WriteString('Config', 'Background-Color', ColorToString(bgColorDialog.Color));
    settingsFile.WriteBool('Config', 'Spoiler', cbSpoiler.Checked);
    settingsFile.WriteString('Config', 'Font-Name', FontDialog.Font.Name);
    settingsFile.WriteInteger('Config', 'Font-Size', FontDialog.Font.Size);
    settingsFile.WriteString('Config', 'Font-Style', StyleToStr(FontDialog.Font.Style));
    //settingsFile.WriteFloat('Config', 'Line-Distance', edtLineDistance.Value);
    settingsFile.WriteBool('Config', 'copy-lyrics-to-clipboard', cbLyricsToClipboard.Checked);
    settingsFile.WriteBool('Config', 'TitleSlide', cbMetaTitleSlide.Checked);
    settingsFile.WriteBool('Config', 'MetaDataFirstSlide', cbMetaDataFirstSlide.Checked);
    settingsFile.WriteBool('Config', 'MetaDataLastSlide', cbMetaDataLastSlide.Checked);
    str := StringReplace(memoMetaData.Lines.Text, LineEnding, '</br>', [rfReplaceAll]);
    settingsFile.WriteString('Config','MetaDataSyntax', str);
    settingsFile.WriteBool('Config', 'BackgroundPicture', cbShowBackgroundImage.Checked);
    settingsFile.WriteString('Config', 'BackgroundPicture-Path', BgPictureDialog.FileName);
    settingsFile.WriteInteger('Config', 'ImageBrightness', sbImageBrightness.Position);
    settingsFile.WriteInteger('Config', 'AutoWrap', seWrapLines.Value);
    settingsFile.WriteInteger('Config', 'AlignHorizontal', comboHorizontal.ItemIndex);
    settingsFile.WriteInteger('Config', 'AlignVertical', comboVertical.ItemIndex);

    Padding := FormPadding.frmSettingsDetailed.ExportPadding;
    settingsFile.WriteInteger('Config', 'Padding-Left', Padding.Left);
    settingsFile.WriteInteger('Config', 'Padding-Top', Padding.Top);
    settingsFile.WriteInteger('Config', 'Padding-Right', Padding.Right);
    settingsFile.WriteInteger('Config', 'Padding-Bottom', Padding.Bottom);

    settingsFile.UpdateFile;
    CanClose := True;
  end;
end;

procedure TfrmSettings.LoadPreviewImage;
begin
  PresentationPreviewCanvas.SlideSettings := self.ExportSlideSettings;
  PresentationPreviewCanvas.PresentationStyleSettings := self.ExportPresentationStyleSettings;
  PresentationPreviewCanvas.Width:=Screen.Width;
  PresentationPreviewCanvas.Height:=Screen.Height;
  if ChangedBackground then
    PresentationPreviewCanvas.LoadBackgroundBitmap;
  PresentationPreviewCanvas.ResizeBackgroundBitmap;
  ImagePresentationPreview.Picture.Assign(PresentationPreviewCanvas.PaintSlide(SlideList[slidelistcur]));
end;

function TFrmSettings.ExportSlideSettings(): TSlideSettings;
var SlideSettings: TSlideSettings;
begin
  SlideSettings.EmptyFrame := cbEmptyFrame.Checked;
  SlideSettings.FirstSlideMeta := cbMetaDataFirstSlide.Checked;
  SlideSettings.LastSlideMeta := cbMetaDataLastSlide.Checked;
  SlideSettings.MaxSlideLineLength:= seWrapLines.Value;
  SlideSettings.MetaSyntax:=memoMetaData.Lines.Text;
  SlideSettings.SpoilerText:=cbSpoiler.Checked;
  SlideSettings.TitleSlide := cbMetaTitleSlide.Checked;
  Result := SlideSettings;
end;

procedure TFrmSettings.ReloadSlideAndPresentationCanvas;
  var DummySongFile: TStringList;
  ExampleSong: TSong;
  PresentationSlideCounter: Integer;
begin
  if Assigned(SlideList) then SlideList.Free;
  SlideList := TSlideList.Create(True);
  DummySongFile := LoadResourceFileIntoStringList('AMAZING GRACE');
  ExampleSong := TSong.Create;
  ExampleSong.importSongFromStringList(DummySongFile);
  DummySongFile.Destroy;
  PresentationSlideCounter := 0;
  SlideList.AddList(CreatePresentationDataFromSong(ExampleSong, frmSettings.ExportSlideSettings(), PresentationSlideCounter));
  PresentationPreviewCanvas.Height:=Screen.Height;
  PresentationPreviewCanvas.Width:=Screen.Width;
  PresentationPreviewCanvas.PresentationStyleSettings := ExportPresentationStyleSettings;
  PresentationPreviewCanvas.LoadBackgroundBitmap;
end;

function TfrmSettings.ExportPresentationStyleSettings: TPresentationStyleSettings;
  var PresentationStyleSettings: TPresentationStyleSettings;
begin
  PresentationStyleSettings.Font:=TFont.Create;
  PresentationStyleSettings.Font.Assign(FontDialog.Font);
  PresentationStyleSettings.BackgroundColor:=bgColorDialog.Color;
  PresentationStyleSettings.ShowBackgroundImage:=cbShowBackgroundImage.Checked;
  PresentationStyleSettings.TextColor:=textColorDialog.Color;
  PresentationStyleSettings.BackgroundImageFilePath:=BgPictureDialog.FileName;
  PresentationStyleSettings.Transparency:=sbImageBrightness.Position;
  PresentationStyleSettings.VerticalAlign:=TTextLayout(comboVertical.ItemIndex);
  PresentationStyleSettings.HorizontalAlign:=THorizontalAlignEnum(comboHorizontal.ItemIndex);
  PresentationStyleSettings.Padding:=FormPadding.frmSettingsDetailed.ExportPadding;
  Result := PresentationStyleSettings;
end;

end.
