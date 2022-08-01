unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, Spin, INIfiles, LCLTranslator;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnBackgroundColor: TButton;
    btnClose: TButton;
    btnFontSizeManually: TButton;
    btnTextColor: TButton;
    cbMetaDataFirstSlide: TCheckBox;
    cbMetaDataLastSlide: TCheckBox;
    cbSpoiler: TCheckBox;
    cbLyricsToClipboard: TCheckBox;
    edtLineDistance: TFloatSpinEdit;
    FontDialog: TFontDialog;
    gbPresentation: TGroupBox;
    lblLineDistance: TLabel;
    lblMeta: TLabel;
    lblMetaContent: TLabel;
    memoMetaData: TMemo;
    textColorDialog: TColorDialog;
    btnSelectDir: TButton;
    cbEmptyFrame: TCheckBox;
    bgColorDialog: TColorDialog;
    edtRepoPath: TEdit;
    labelSongDir: TLabel;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure btnFontSizeManuallyClick(Sender: TObject);
    procedure btnSelectDirClick(Sender: TObject);
    procedure btnBackgroundColorClick(Sender: TObject);
    procedure btnTextColorClick(Sender: TObject);
    procedure cbLyricsToClipboardChange(Sender: TObject);
    procedure edtRepoPathChange(Sender: TObject);
    procedure edtRepoPathEditingDone(Sender: TObject);
    procedure edtRepoPathExit(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure labelSongDirClick(Sender: TObject);
    procedure lblMetaClick(Sender: TObject);
    procedure loadSettings();
  private
    { private declarations }
    procedure LocaliseCaptions;
  public
    { public declarations }
  end;

var
  frmSettings: TfrmSettings;
  settingsFile: TINIFile;

{ResourceString
  strLiederverzeichnis = 'Liederverzeichnis';
  strLeerfolieZwischenLiedern = 'Leerfolie zwischen Liedern';
  strNaechsteFolieAndeuten = 'Nächste Folie andeuten';
  strZeilenabstand = 'Zeilenabstand';
  strPraesentationsanzeige = 'Präsentationsanzeige';
  strSchriftgroesse = 'Schriftgröße und -art';
  strHintergrundfarbe = 'Hintergrundfarbe';
  strTextfarbe = 'Textfarbe';
  strSchliessen = 'Schließen';
  strFormCaption = 'Einstellungen';
  strSongLyricsToClipboard = 'Kopiere die Liedtexte in die Zwischenablage'; }

implementation

Uses
  Present, SongSelection;

{$R *.lfm}

{ TfrmSettings }

procedure TfrmSettings.LocaliseCaptions;
begin
  {labelSongDir.Caption := StrLiederverzeichnis + ': ';
  cbEmptyFrame.Caption := strLeerfolieZwischenLiedern;
  cbSpoiler.Caption := strNaechsteFolieAndeuten;
  cbLyricsToClipboard.Caption:=strSongLyricsToClipboard;
  lblLineDistance.Caption:= strZeilenabstand;
  lblPresentationView.Caption:=strPraesentationsanzeige;
  btnFontSizeManually.Caption:= strSchriftgroesse + '...';
  btnBackgroundColor.Caption := strHintergrundfarbe + '...';
  btnTextColor.Caption := strTextfarbe + '...';
  btnClose.Caption := strSchliessen;
  self.Caption:= strFormCaption;}
end;

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

procedure TfrmSettings.loadSettings();
begin
  edtRepoPath.Text := settingsFile.ReadString('Config', 'Repo-Path', getRepoDir());
  cbEmptyFrame.Checked := settingsFile.ReadBool('Config', 'empty-Frame', True);
  cbLyricsToClipboard.Checked := settingsFile.ReadBool('Config', 'copy-lyrics-to-clipboard', True);
  textColorDialog.Color := StringToColor(settingsFile.ReadString('Config', 'Text-Color', 'clWhite'));
  bgColorDialog.Color := StringToColor(settingsFile.ReadString('Config', 'Background-Color', 'clBlack'));
  cbSpoiler.Checked:=settingsFile.ReadBool('Config', 'Spoiler', True);
  FontDialog.Font.Name:=settingsFile.ReadString('Config', 'Font-Name', 'default');
  FontDialog.Font.Style := StrToStyle(settingsFile.ReadString('Config', 'Font-Style', 'ssss'));
  FontDialog.Font.Size:= settingsFile.ReadInteger('Config', 'Font-Size', 42);
  edtLineDistance.Value:=settingsFile.ReadFloat('Config', 'Line-Distance', 1);
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  self.LocaliseCaptions;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin

end;

procedure TfrmSettings.labelSongDirClick(Sender: TObject);
begin

end;

procedure TfrmSettings.lblMetaClick(Sender: TObject);
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

procedure TfrmSettings.btnBackgroundColorClick(Sender: TObject);
begin
  bgColorDialog.Execute;
end;

procedure TfrmSettings.btnTextColorClick(Sender: TObject);
begin
  textColorDialog.Execute;
end;

procedure TfrmSettings.cbLyricsToClipboardChange(Sender: TObject);
begin

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
  frmPresent.loadSettings;
  if (ProgrammMode = ModeMultiScreenPresentation) Then SongSelection.frmSongs.ImageUpdater.Enabled:=True;
end;


procedure TfrmSettings.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
  if DirectoryExists(edtRepoPath.Text) = False then Application.MessageBox('Sie müssen ein gültiges Verzeichnis angeben', 'Fehler')
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
    settingsFile.WriteFloat('Config', 'Line-Distance', edtLineDistance.Value);
    settingsFile.WriteBool('Config', 'copy-lyrics-to-clipboard', cbLyricsToClipboard.Checked);
    settingsFile.UpdateFile;
    CanClose := True;
  end;
end;

end.

