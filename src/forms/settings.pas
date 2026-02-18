unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, Spin, INIfiles, LCLTranslator, DefaultTranslator, ExtDlgs,
  LCLINTF, LCLType, ExtCtrls, ActnList, Present, Lyrics, Slides,
  ResourceHandling, PresentationCanvas, settingspadding, loadimagethread,
  PresentationModels, CantaraFontDialog, settingshandler;

type

  { TfrmSettings }

  TfrmSettings = class(TForm, ISettingsHandler)
    BgPictureFileDialog: TOpenDialog;
    btnBackgroundColor: TButton;
    btnClose: TButton;
    btnFontSizeManually: TButton;
    btnTextColor: TButton;
    btnBackgroundImage: TButton;
    btnDetails: TButton;
    cbBlackScreenOnEmpty: TCheckBox;
    cbFadeTransition: TCheckBox;
    lblFadeMs: TLabel;
    seFadeDuration: TSpinEdit;
    cbMetaDataFirstSlide: TCheckBox;
    cbMetaTitleSlide: TCheckBox;
    cbShowBackgroundImage: TCheckBox;
    cbMetaDataLastSlide: TCheckBox;
    cbSpoiler: TCheckBox;
    comboVertical: TComboBox;
    comboHorizontal: TComboBox;
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
    procedure btnBackgroundImageClick(Sender: TObject);
    procedure cbBlackScreenOnEmptyChange(Sender: TObject);
    procedure cbFadeTransitionChange(Sender: TObject);
    procedure btnDetailsClick(Sender: TObject);
    procedure btnFontSizeManuallyClick(Sender: TObject);
    procedure btnSelectDirClick(Sender: TObject);
    procedure btnBackgroundColorClick(Sender: TObject);
    procedure btnTextColorClick(Sender: TObject);
    procedure cbEmptyFrameChange(Sender: TObject);
    procedure cbMetaDataFirstSlideChange(Sender: TObject);
    procedure cbMetaDataLastSlideChange(Sender: TObject);
    procedure cbMetaTitleSlideChange(Sender: TObject);
    procedure cbShowBackgroundImageChange(Sender: TObject);
    procedure cbSpoilerChange(Sender: TObject);
    procedure comboHorizontalChange(Sender: TObject);
    procedure comboVerticalChange(Sender: TObject);
    procedure edtRepoPathEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gbPresentationClick(Sender: TObject);
    procedure ImagePresentationPreviewClick(Sender: TObject);
    procedure loadSettings();
    procedure memoMetaDataChange(Sender: TObject);
    procedure memoMetaDataEditingDone(Sender: TObject);
    procedure sbImageBrightnessChange(Sender: TObject);
    procedure sbImageBrightnessDragOver(Sender, Source: TObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure sbImageBrightnessEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure sbImageBrightnessEnter(Sender: TObject);
    procedure sbImageBrightnessExit(Sender: TObject);
    procedure seWrapLinesChange(Sender: TObject);
    procedure UpdatePreviewTimerTimer(Sender: TObject);
  private
    { private declarations }
    PresentationPreviewCanvas: TPresentationCanvasHandler;
    SlideList: TSlideList;
    slidelistcur: Integer;
    ExampleSong: TSong;
    LoadImageThread: TLoadImageThread;
    procedure LoadPreviewImage;
    procedure ReloadSlideAndPresentationCanvas;
    procedure AdjustImageBrightnessText;
  public
    { public declarations }
    changedBackground: Boolean;
    {Exports the slide settings as TSlideSettings record }
    function ExportSlideSettings: TSlideSettings;
    function ExportPresentationStyleSettings: TPresentationStyleSettings;
    function GetRepositoryPath: String;
    function GetSettingsFile: TINIFile;
    procedure SetPortalsUsedFlag;
  end;

  { This function returns the path of the folder with the default pictures.
  If the folder can not be found, the returning string will be empty. }
function GetDefaultPictureDir: String;

var
  frmSettings: TfrmSettings;
  settingsFile: TINIFile;

resourcestring
  strTransparency = 'Increase transparency by ';
  strBrightness = 'Increase brightness by ';
  strPictureOriginalState = 'Picture is shown as it is';
  strErrorCaption = 'Error';
  strValidSongRepository = 'Please choose a valid folder for the song repository!';
  strDefaultMetaTemplate = '{%author%}Author: {author}' + LineEnding +
    '{%bible%}Bible Reference: {bible}' + LineEnding +
    '{%ccli-songnumber%}CCLI song number: {ccli-songnumber} | License Number: {ccli-licensenumber}';
  strNoBackgroundImageSelected = 'You need to select a background image first.';

implementation

uses
  SongSelection;

  {$R *.lfm}

  { TfrmSettings }

function getRepoDir(): String;
begin
  if DirectoryExists(getUserdir() + 'Liederverzeichnis') then
    Result := getUserdir() + 'Liederverzeichnis'
  else if DirectoryExists(getUserdir() + 'Dokumente' + PathDelim +
    'Liederverzeichnis') then
    Result := getUserdir() + 'Dokumente' + PathDelim + 'Liederverzeichnis'
  else if DirectoryExists(ExtractFilePath(Application.ExeName) +
    'Liederverzeichnis') then
    Result := ExtractFilePath(Application.ExeName) + 'Liederverzeichnis'
  else
    Result := '';
end;

function StyleToStr(Style: TFontStyles): String;
begin
  SetLength(Result, 4);
  {T = true, F = false 83 is ordinal value of S, if true then S + 1 (84) = T}
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

function GetDefaultPictureDir: String;
begin
  Result := '';
  if DirectoryExists('backgrounds') then Result := 'backgrounds';
  {$IFDEF LINUX }
  if DirectoryExists('/usr/share/cantara/backgrounds') then Result := '/usr/share/cantara/backgrounds';
  {$ENDIF }
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
var
  DummySongFile: TStringList;
  PictureDir: String;
begin
  changedBackground := False;
  PresentationPreviewCanvas := TPresentationCanvasHandler.Create;
  slidelistcur := 0;
  DummySongFile := LoadResourceFileIntoStringList('AMAZING GRACE');
  ExampleSong := TSong.Create;
  ExampleSong.MetaDict.Add('title', 'Amazing Grace');
  ExampleSong.importSongFromStringList(DummySongFile);
  DummySongFile.Destroy;
  SlideList := TSlideList.Create(True);
  PictureDir := GetDefaultPictureDir;
  if PictureDir <> '' then BgPictureDialog.InitialDir := PictureDir;
end;

procedure TfrmSettings.FormDestroy(Sender: TObject);
begin
  PresentationPreviewCanvas.Destroy;
  ExampleSong.Destroy;
  SlideList.Destroy;

  SettingsFile.Destroy;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  LoadImageThread := TLoadImageThread.Create(True);
  LoadImageThread.Start;
  sbImageBrightnessChange(frmSettings);
  changedBackground := False;
  AdjustImageBrightnessText;
  ReloadSlideAndPresentationCanvas;
end;

procedure TfrmSettings.gbPresentationClick(Sender: TObject);
begin
  PresentationPreviewCanvas := TPresentationCanvasHandler.Create;
end;

procedure TfrmSettings.ImagePresentationPreviewClick(Sender: TObject);
begin
  if slideListCur < SlideList.Count - 1 then slidelistcur += 1
  else
    slideListCur := 0;
  LoadPreviewImage;
end;

procedure TfrmSettings.btnSelectDirClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
  begin
    edtRepoPath.Text := SelectDirectoryDialog.FileName;
    {$IF defined(CONTAINER)}
    Self.SetPortalsUsedFlag;
    {$ENDIF}
  end;
end;

procedure TfrmSettings.btnFontSizeManuallyClick(Sender: TObject);
begin
  CFontDialog.ShowModal;
  LoadPreviewImage;
end;

procedure TfrmSettings.btnDetailsClick(Sender: TObject);
begin
  FormPadding.ShowModal;
  LoadPreviewImage;
end;

procedure TfrmSettings.btnBackgroundImageClick(Sender: TObject);
var
  UsedOpenDialog: TOpenDialog;
begin
  {
   On containerized environments like flatpak, the file chooser dialog is used
   as portal to grant permissions. The Lazarus TPictureDialog is not supported by Qt
   and would therefore not find any file under Flatpak. That is why we have to
   use a normal TOpenFileDialog under Flatpak.
  }
  {$IF defined(CONTAINER)}
  UsedOpenDialog := BgPictureFileDialog as TOpenDialog;
  {$ELSE}
  UsedOpenDialog := BgPictureDialog;
  {$ENDIF}
  if (UsedOpenDialog.Execute) And (FileExists(UsedOpenDialog.FileName)) then
  begin
    {$IF defined(CONTAINER)}
    BgPictureDialog.FileName := UsedOpenDialog.FileName;
    {$ENDIF}
    changedBackground := True;
    cbShowBackgroundImage.Checked := True;
    LoadPreviewImage;
  end
  else if Not (FileExists(UsedOpenDialog.FileName)) then
    cbShowBackgroundImage.Checked := False;
end;

procedure TfrmSettings.btnBackgroundColorClick(Sender: TObject);
begin
  bgColorDialog.Execute;
  LoadPreviewImage;
end;

procedure TfrmSettings.btnTextColorClick(Sender: TObject);
begin
  textColorDialog.Execute;
  LoadPreviewImage;
end;

procedure TfrmSettings.cbEmptyFrameChange(Sender: TObject);
begin
  ReloadSlideAndPresentationCanvas;
end;

procedure TfrmSettings.cbBlackScreenOnEmptyChange(Sender: TObject);
begin
  LoadPreviewImage;
end;

procedure TfrmSettings.cbFadeTransitionChange(Sender: TObject);
begin
  seFadeDuration.Enabled := cbFadeTransition.Checked;
  LoadPreviewImage;
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
  if (cbShowBackgroundImage.Checked = True) And
    (Not (FileExists(BgPictureDialog.FileName))) then
  begin
    cbShowBackgroundImage.Checked := False;
    Application.MessageBox(PChar(strNoBackgroundImageSelected),
      PChar(strHint), mb_OK + mb_IconError);
    Exit;
  end;
  //btnBackgroundImage.Enabled:=cbShowBackgroundImage.Checked;
  changedBackground := True;
  LoadPreviewImage;
end;

procedure TfrmSettings.cbSpoilerChange(Sender: TObject);
begin
  ReloadSlideAndPresentationCanvas;
end;

procedure TfrmSettings.comboHorizontalChange(Sender: TObject);
begin
  LoadPreviewImage;
end;

procedure TfrmSettings.comboVerticalChange(Sender: TObject);
begin
  LoadPreviewImage;
end;

procedure TfrmSettings.edtRepoPathEditingDone(Sender: TObject);
begin
  if (edtRepoPath.Text <> '') And (edtRepoPath.Text[length(edtRepoPath.Text)] =
    PathDelim) then
    edtRepoPath.Text := copy(edtRepoPath.Text, 1, length(edtRepoPath.Text) - 1);
end;

procedure TfrmSettings.FormClose(Sender: TObject);
begin
  // Prevent exceptions from happening
  if Not FileExists(BgPictureDialog.FileName) then
  begin
    cbShowBackgroundImage.Checked := False;
    cbShowBackgroundImageChange(frmSettings);
  end;

  //if changedBackground then frmPresent.loadSettings;
  frmSongs.edtSearch.Text := '';
  LoadImageThread.Terminate;
  LoadImageThread.Destroy;
end;

procedure TfrmSettings.loadSettings();
var
  str: String;
  Padding: TPadding;
begin
  edtRepoPath.Text := settingsFile.ReadString('Config', 'Repo-Path', getRepoDir());
  cbEmptyFrame.Checked := settingsFile.ReadBool('Config', 'empty-Frame', True);
  cbBlackScreenOnEmpty.Checked := settingsFile.ReadBool('Config', 'BlackScreenOnEmpty', False);
  cbFadeTransition.Checked := settingsFile.ReadBool('Config', 'FadeTransition', False);
  seFadeDuration.Value := settingsFile.ReadInteger('Config', 'FadeDurationMs', 300);
  seFadeDuration.Enabled := cbFadeTransition.Checked;
  textColorDialog.Color := StringToColor(settingsFile.ReadString('Config',
    'Text-Color', 'clWhite'));
  bgColorDialog.Color := StringToColor(settingsFile.ReadString('Config',
    'Background-Color', 'clBlack'));
  cbSpoiler.Checked := settingsFile.ReadBool('Config', 'Spoiler', True);
  cbMetaTitleSlide.Checked := settingsFile.ReadBool('Config', 'TitleSlide', False);
  cbMetaDataFirstSlide.Checked :=
    settingsFile.ReadBool('Config', 'MetaDataFirstSlide', False);
  cbMetaDataLastSlide.Checked :=
    settingsFile.ReadBool('Config', 'MetaDataLastSlide', False);
  str := settingsFile.ReadString('Config', 'MetaDataSyntax', strDefaultMetaTemplate);
  memoMetaData.Lines.Text := StringReplace(str, '</br>', LineEnding, [rfReplaceAll]);
  CFontDialog.SelectedFont.Name := settingsFile.ReadString('Config', 'Font-Name', 'default');
  CFontDialog.SelectedFont.Size := settingsFile.ReadInteger('Config', 'Font-Size', 42);
  CFontDialog.SelectedFont.Style := StrToStyle(settingsFile.ReadString('Config',
    'Font-Style', 'FFFF'));
  BgPictureDialog.FileName := settingsFile.ReadString('Config',
    'BackgroundPicture-Path', '');
  cbShowBackgroundImage.Checked :=
    settingsFile.ReadBool('Config', 'BackgroundPicture', False);
  // must be after filename
  cbShowBackgroundImageChange(frmSettings);
  sbImageBrightness.Position :=
    Abs(settingsFile.ReadInteger('Config', 'ImageBrightness', 0));
  seWrapLines.Value := settingsFile.ReadInteger('Config', 'AutoWrap', 4);
  comboHorizontal.ItemIndex :=
    settingsFile.ReadInteger('Config', 'AlignHorizontal', Ord(Align_Center));
  // default is centering
  comboVertical.ItemIndex := settingsFile.ReadInteger('Config',
    'AlignVertical', Ord(tlCenter)); // default is in the middle

  Padding.Left := settingsFile.ReadInteger('Config', 'Padding-Left',
    PresentationCanvas.PADDING);
  Padding.Right := settingsFile.ReadInteger('Config', 'Padding-Right',
    PresentationCanvas.PADDING);
  Padding.Top := settingsFile.ReadInteger('Config', 'Padding-Top',
    PresentationCanvas.PADDING);
  Padding.Bottom := settingsFile.ReadInteger('Config', 'Padding-Bottom',
    PresentationCanvas.PADDING);
  FormPadding.frmSettingsDetailed.ImportPadding(Padding);
  AdjustImageBrightnessText;
end;

procedure TfrmSettings.memoMetaDataChange(Sender: TObject);
begin
  ReloadSlideAndPresentationCanvas;
end;

procedure TfrmSettings.memoMetaDataEditingDone(Sender: TObject);
begin
  ReloadSlideAndPresentationCanvas;
end;

procedure TfrmSettings.sbImageBrightnessChange(Sender: TObject);
begin
  if sbImageBrightness.Position > 0 then
    lblImageExplainer.Caption :=
      strTransparency + ' ' + IntToStr(Abs(sbImageBrightness.Position)) + '%'
  else if sbImageBrightness.Position = 0 then
    lblImageExplainer.Caption := strPictureOriginalState;

  ChangedBackground := True;
  LoadPreviewImage;
end;

procedure TfrmSettings.sbImageBrightnessDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  ChangedBackground := True;
  LoadPreviewImage;
end;

procedure TfrmSettings.sbImageBrightnessEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  ChangedBackground := True;
  LoadPreviewImage;
end;

procedure TfrmSettings.sbImageBrightnessEnter(Sender: TObject);
begin
  ChangedBackground := True;
  LoadPreviewImage;
end;

procedure TfrmSettings.sbImageBrightnessExit(Sender: TObject);
begin
  ChangedBackground := True;
  LoadPreviewImage;
end;

procedure TfrmSettings.seWrapLinesChange(Sender: TObject);
begin
  if seWrapLines.Value < 0 then seWrapLines.Value := 0;
  try
    ReloadSlideAndPresentationCanvas;
  finally
    LoadPreviewImage;
  end;
end;

procedure TfrmSettings.UpdatePreviewTimerTimer(Sender: TObject);
begin
  LoadPreviewImage;
end;

procedure TfrmSettings.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  str: String;
  Padding: TPadding;
begin
  CanClose := False;
  if DirectoryExists(edtRepoPath.Text) = False then
    Application.MessageBox(PChar(strValidSongRepository), PChar(strErrorCaption))
  else
  begin
    try
      settingsFile.WriteString('Config', 'Repo-Path', edtRepoPath.Text);
      settingsFile.WriteBool('Config', 'empty-Frame', cbEmptyFrame.Checked);
      settingsFile.WriteBool('Config', 'BlackScreenOnEmpty', cbBlackScreenOnEmpty.Checked);
      settingsFile.WriteBool('Config', 'FadeTransition', cbFadeTransition.Checked);
      settingsFile.WriteInteger('Config', 'FadeDurationMs', seFadeDuration.Value);
      settingsFile.WriteString('Config', 'Text-Color',
        ColorToString(textColorDialog.Color));
      settingsFile.WriteString('Config', 'Background-Color',
        ColorToString(bgColorDialog.Color));
      settingsFile.WriteBool('Config', 'Spoiler', cbSpoiler.Checked);
      settingsFile.WriteString('Config', 'Font-Name', CFontDialog.SelectedFont.Name);
      settingsFile.WriteInteger('Config', 'Font-Size', CFontDialog.SelectedFont.Size);
      settingsFile.WriteString('Config', 'Font-Style', StyleToStr(CFontDialog.SelectedFont.Style));
      //settingsFile.WriteFloat('Config', 'Line-Distance', edtLineDistance.Value);
      settingsFile.WriteBool('Config', 'TitleSlide', cbMetaTitleSlide.Checked);
      settingsFile.WriteBool('Config', 'MetaDataFirstSlide', cbMetaDataFirstSlide.Checked);
      settingsFile.WriteBool('Config', 'MetaDataLastSlide', cbMetaDataLastSlide.Checked);
      str := StringReplace(memoMetaData.Lines.Text, LineEnding, '</br>', [rfReplaceAll]);
      settingsFile.WriteString('Config', 'MetaDataSyntax', str);
      settingsFile.WriteString('Config', 'BackgroundPicture-Path',
        BgPictureDialog.FileName);

      // MUST be before background picture
      settingsFile.WriteBool('Config', 'BackgroundPicture', cbShowBackgroundImage.Checked);
      settingsFile.WriteInteger('Config', 'ImageBrightness', sbImageBrightness.Position);
      settingsFile.WriteInteger('Config', 'AutoWrap', seWrapLines.Value);
      settingsFile.WriteInteger('Config', 'AlignHorizontal', comboHorizontal.ItemIndex);
      settingsFile.WriteInteger('Config', 'AlignVertical', comboVertical.ItemIndex);

      Padding := FormPadding.frmSettingsDetailed.ExportPadding;
      settingsFile.WriteInteger('Config', 'Padding-Left', Padding.Left);
      settingsFile.WriteInteger('Config', 'Padding-Top', Padding.Top);
      settingsFile.WriteInteger('Config', 'Padding-Right', Padding.Right);
      settingsFile.WriteInteger('Config', 'Padding-Bottom', Padding.Bottom);
    finally
      settingsFile.UpdateFile;
      CanClose := True;
    end;
  end;
end;

procedure TfrmSettings.LoadPreviewImage;
begin
  if (Not Assigned(LoadImageThread)) Or (self.SlideList.Count <= 0) then Exit;
  try
    LoadImageThread.LoadData(self.ExportPresentationStyleSettings,
      self.ExportSlideSettings,
      PresentationPreviewCanvas,
      Screen,
      ChangedBackground,
      self.SlideList[slidelistcur],
      ImagePresentationPreview.Picture);
    LoadImageThread.RunOnce;
  finally
  end;
end;

function TfrmSettings.ExportSlideSettings: TSlideSettings;
var
  SlideSettings: TSlideSettings;
begin
  SlideSettings.EmptySlideBetweenSongs := cbEmptyFrame.Checked;
  SlideSettings.FirstSlideMeta := cbMetaDataFirstSlide.Checked;
  SlideSettings.LastSlideMeta := cbMetaDataLastSlide.Checked;
  SlideSettings.MaxSlideLineLength := seWrapLines.Value;
  SlideSettings.MetaSyntax := memoMetaData.Lines.Text;
  SlideSettings.SpoilerText := cbSpoiler.Checked;
  SlideSettings.TitleSlide := cbMetaTitleSlide.Checked;
  Result := SlideSettings;
end;

procedure TfrmSettings.ReloadSlideAndPresentationCanvas;
var
  PresentationSlideCounter: Integer;
  SlideSettings: TSlideSettings;
  AmazingGraceSlideList: TSlideList;
begin
  SlideListCur := 0; // to prevent exceptions if something cant be found anymore
  SlideSettings := self.ExportSlideSettings;
  SlideList.Clear;
  PresentationSlideCounter := 0;
  AmazingGraceSlideList := CreatePresentationDataFromSong(ExampleSong,
    SlideSettings, PresentationSlideCounter);
  SlideList.AddList(AmazingGraceSlideList);
  AmazingGraceSlideList.Destroy;
  LoadPreviewImage;
end;

procedure TfrmSettings.AdjustImageBrightnessText;
begin
  if sbImageBrightness.Position > 0 then
    lblImageExplainer.Caption :=
      strTransparency + ' ' + IntToStr(Abs(sbImageBrightness.Position)) + '%'
  else if sbImageBrightness.Position = 0 then
    lblImageExplainer.Caption := strPictureOriginalState;
end;

function TfrmSettings.ExportPresentationStyleSettings: TPresentationStyleSettings;
var
  PresentationStyleSettings: TPresentationStyleSettings;
begin
  PresentationStyleSettings.Font := TFont.Create;
  PresentationStyleSettings.Font.Assign(CFontDialog.SelectedFont);
  PresentationStyleSettings.BackgroundColor := bgColorDialog.Color;
  PresentationStyleSettings.ShowBackgroundImage := cbShowBackgroundImage.Checked;
  PresentationStyleSettings.TextColor := textColorDialog.Color;
  PresentationStyleSettings.BackgroundImageFilePath := BgPictureDialog.FileName;
  PresentationStyleSettings.Transparency := -Abs(sbImageBrightness.Position);
  PresentationStyleSettings.VerticalAlign := TTextLayout(comboVertical.ItemIndex);
  PresentationStyleSettings.HorizontalAlign :=
    THorizontalAlignEnum(comboHorizontal.ItemIndex);
  PresentationStyleSettings.Padding := FormPadding.frmSettingsDetailed.ExportPadding;
  PresentationStyleSettings.BlackScreenOnEmptySlide := cbBlackScreenOnEmpty.Checked;
  PresentationStyleSettings.FadeTransition := cbFadeTransition.Checked;
  PresentationStyleSettings.FadeDurationMs := seFadeDuration.Value;
  Result := PresentationStyleSettings;
end;

function TfrmSettings.GetRepositoryPath: String;
begin
  Result := edtRepoPath.Text;
end;

function TfrmSettings.GetSettingsFile: TINIFile;
begin
  Result := SettingsFile;
end;

procedure TfrmSettings.SetPortalsUsedFlag;
begin
  SettingsFile.WriteBool('Flathub', 'Portals used', true);
  SettingsFile.UpdateFile;
end;

end.
