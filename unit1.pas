unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs, StrUtils,
  StdCtrls, ExtCtrls, Buttons, Menus, Present, settings, info, INIFiles, DefaultTranslator, Clipbrd;

type
  TRepoFile = record
    Name: string;
    filePath: string;
  end;

  TSongPosition = record
    songname: string;
    stanzaposition: integer;
    songposition: integer;
    stanzapositionstart: integer;
  end;
  { TfrmSongs }

  TfrmSongs = class(TForm)
    btnAdd: TButton;
    btnQuitPresentation: TButton;
    btnRemove: TButton;
    btnUp: TButton;
    btnDown: TButton;
    btnStartPresentation: TButton;
    btnSettings: TButton;
    btnClear: TButton;
    btnGoLeft: TButton;
    btnGoRight: TButton;
    chkMultiWindowMode: TCheckBox;
    edtSearch: TEdit;
    grbSettings: TGroupBox;
    grbControl: TGroupBox;
    imgLiveViewer: TImage;
    lblFoilNumber: TLabel;
    lblPresentation: TLabel;
    lbxSRepo: TListBox;
    lbxSselected: TListBox;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    itemLoad: TMenuItem;
    itemSave: TMenuItem;
    itemSeperator1: TMenuItem;
    itemEnd: TMenuItem;
    menuEdit: TMenuItem;
    itemSettings: TMenuItem;
    menuHelp: TMenuItem;
    itemAbout: TMenuItem;
    itemPresentation: TMenuItem;
    itemReloadSongList: TMenuItem;
    OpenDialog: TOpenDialog;
    Control: TPanel;
    pnlMultiScreen: TPanel;
    PnlSplitter: TSplitter;
    SaveDialog: TSaveDialog;
    ImageUpdater: TTimer;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnGoLeftClick(Sender: TObject);
    procedure btnGoRightClick(Sender: TObject);
    procedure btnQuitPresentationClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnStartPresentationClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure BtnUpdateClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grbControlClick(Sender: TObject);
    procedure grbSettingsClick(Sender: TObject);
    procedure ImageUpdaterTimer(Sender: TObject);
    procedure itemEndClick(Sender: TObject);
    procedure itemLoadClick(Sender: TObject);
    procedure itemSaveClick(Sender: TObject);
    procedure lbxSRepoClick(Sender: TObject);
    procedure lbxSRepoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure lbxSselectedClick(Sender: TObject);
    procedure lbxSselectedDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxSselectedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbxSselectedKeyPress(Sender: TObject; var Key: char);
    procedure lbxSselectedResize(Sender: TObject);
    procedure loadRepo(repoPath: string);
    procedure itemAboutClick(Sender: TObject);
    procedure itemReloadSongListClick(Sender: TObject);
    procedure pnlMultiScreenClick(Sender: TObject);
    procedure pnlMultiScreenResize(Sender: TObject);
    procedure PnlSplitterCanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);
    procedure PnlSplitterMoved(Sender: TObject);
    procedure CreatePresentationData;
    function GetCurrentSongPosition: TSongPosition;
    procedure UpdateSongPositionInLbxSSelected;
  private
    { private declarations }
    procedure LocaliseCaptions;
    procedure FilterListBox(s: String);
    procedure ReloadPresentationImage;
    procedure BringToFront;
  public
    { public declarations }
  end;

const
  ModeSelection = 'S';
  ModeSingleScreenPresentation = 'P';
  ModeMultiscreenPresentation = 'M';

var
  frmSongs: TfrmSongs;
  repo: array of TRepoFile;
  ProgrammMode: char;

ResourceString
  StrErsteBenutzung = 'Sie nutzen dieses Programm zum ersten Mal. Bitte wählen Sie einen Ordner aus, in dem sich die Liedtexte befinden.';
  StrFehlerOeffnen = 'Fehler beim Öffnen. Wahrscheinlich haben Sie nicht die nötigen Rechte, um auf diese Datei zuzugreifen';
  StrFehlerSpeichern = 'Fehler beim Speichern. Wahrscheinlich haben Sie nicht die nötigen Rechte, um auf diesen Ordner zuzugreifen.';
  StrFehlerKeineLiederBeiPraesentation = 'Es müssen zuerst Lieder hinzugefügt werden.';
  StrButtonPraesentation = 'Präsentation...';
  StrButtonEinstellungen = 'Einstellungen...';
  StrMenuDatei = 'Datei';
  StrMenuBearbeiten = 'Bearbeiten';
  StrMenuHilfe = 'Hilfe';
  StrMenuAuswahlLaden = 'Auswahl laden...';
  StrMenuAuswahlSpeichern = 'Auswahl speichern...';
  StrMenuLiederlisteNeuLaden = 'Liederliste neu laden';
  StrMenuPraesentation = 'Präsentation...';
  StrMenuBeenden = 'Beenden';
  StrMenuEinstellungen = 'Einstellungen...';
  StrMenuInfo = 'Informationen zum Programm...';
  StrFormCaption = 'Liedauswahl (Cantara)';
  StrSearchFieldHint = 'Suchen...';
  StrMultipleWindowMode = 'Zwei-Fenster-Modus (für zwei Bildschirme und Videokonferenzen geeignet)';
  StrFolie = 'Folie';

implementation

{$R *.lfm}

{ TfrmSongs }

procedure TfrmSongs.LocaliseCaptions;
begin
  btnStartPresentation.Caption := StrButtonPraesentation;
  btnSettings.Caption := StrButtonEinstellungen;
  menuFile.Caption := StrMenuDatei;
  itemLoad.Caption := StrMenuAuswahlLaden;
  itemSave.Caption := StrMenuAuswahlSpeichern;
  itemPresentation.Caption := StrMenuPraesentation;
  itemEnd.Caption := StrMenuBeenden;
  menuEdit.Caption := StrMenuBearbeiten;
  itemSettings.Caption := StrMenuEinstellungen;
  menuHelp.Caption := StrMenuHilfe;
  itemAbout.Caption := StrMenuInfo;
  self.Caption:=StrFormCaption;
  self.edtSearch.TextHint := StrSearchFieldHint;
  chkMultiWindowMode.Caption := StrMultipleWindowMode;
end;

procedure TfrmSongs.loadRepo(repoPath: string);
var Info: TSearchRec;
    i,c: integer;
    songName: string;
begin
  if FindFirst(repoPath + PathDelim + '*', faAnyFile, Info)=0 then
    begin
    lbxSRepo.Clear;
    setlength(repo, 0);
    Repeat
      if (Info.Name[1] <> '.') then
        begin
         // Finde den letzten Punkt
         songName := Info.Name + '.';
         i :=-1;
         for i := 1 to length(Info.Name) do
           if songName[i] = '.' then c := i;
         // Entferne die Dateiendung
         songName := copy(songName,1,c-1);
         lbxSRepo.Items.Add(songName);
         setlength(repo, length(repo)+1);
         // Füllen des Repo-Arrays zur späteren Fehlerkorrektur!
         repo[(length(repo)-1)].Name := songName;
         repo[(length(repo)-1)].filePath := Info.Name;
        end;
    Until FindNext(info)<>0;
    end;
  FindClose(Info);
end;

procedure TfrmSongs.itemAboutClick(Sender: TObject);
begin
  frmInfo.ShowModal;
end;

procedure TfrmSongs.itemReloadSongListClick(Sender: TObject);
begin
  lbxSRepo.Items.Clear;
  loadRepo(frmSettings.edtRepoPath.Text);
end;

procedure TfrmSongs.pnlMultiScreenClick(Sender: TObject);
begin

end;

procedure TfrmSongs.pnlMultiScreenResize(Sender: TObject);
begin
  imgLiveViewer.Width := pnlMultiScreen.Width;
  // Falls Bild noch nicht da, nehme FrmPresent Höhe
  if imgLiveViewer.Height < 10 Then
    imgLiveViewer.Height := frmPresent.Height
  // Passe Höhe an Bild an
  else if (imgLiveViewer.Picture.Bitmap.Width > 0) Then
    imgLiveViewer.Height := round(imgLiveViewer.Width * imgLiveViewer.Picture.Bitmap.Height / imgLiveViewer.Picture.Bitmap.Width);
end;

procedure TfrmSongs.PnlSplitterCanOffset(Sender: TObject;
  var NewOffset: Integer; var Accept: Boolean);
begin

end;

procedure TfrmSongs.PnlSplitterMoved(Sender: TObject);
begin
  frmSongs.FormResize(self);
end;

procedure TfrmSongs.FormResize(Sender: TObject);
begin
  //grbControl.Width:=40;
  lbxSRepo.Height := frmSongs.Height - lbxSRepo.Top - grbSettings.Height - MainMenu.Height;
  grbControl.Height := frmSongs.Height - grbControl.Top - grbSettings.Height - MainMenu.Height;
  lbxSSelected.Height := frmSongs.Height - lbxSSelected.Top - grbSettings.Height - MainMenu.Height;
  edtSearch.Top := 0;
  edtSearch.Left := 0;
  {$if defined(LINUX)}
  chkMultiWindowMode.Top := Round((grbSettings.Height-chkMultiWindowMode.Height)/2);
  {$endif}
  {$if defined(WINDOWS)}
  chkMultiWindowMode.Top := btnStartPresentation.Top;
  {$endif}
  if (ProgrammMode = ModeSelection) OR (ProgrammMode = ModeSingleScreenPresentation) Then
  Begin
    lbxSRepo.Width:=(frmSongs.Width-grbControl.Width) div 2;
    lbxSSelected.left:=grbControl.Width+lbxSRepo.Width;
    lbxSSelected.Width:=lbxSRepo.Width;
    edtSearch.Width:= lbxSRepo.Width;
    lbxSRepo.BorderSpacing.Top := edtSearch.Height;
    grbControl.Left := lbxSRepo.Left + lbxSRepo.Width;
    PnlSplitter.Visible := False;
    pnlMultiScreen.Visible := False;
  end else
  Begin
    lbxSRepo.Width:=(frmSongs.Width-grbControl.Width-pnlMultiscreen.Width) div 2;
    lbxSSelected.Width:=lbxSRepo.Width;
    lbxSSelected.left:=grbControl.Width+lbxSRepo.Width;
    edtSearch.Width:= lbxSRepo.Width;
    lbxSRepo.BorderSpacing.Top := edtSearch.Height;
    grbControl.Left := lbxSRepo.Left + lbxSRepo.Width;
    pnlMultiScreen.Visible := True;
    PnlSplitter.Visible := True;
    pnlMultiScreen.Left := frmSongs.Width-pnlMultiScreen.Width;
    itemPresentation.Enabled := False;
    btnStartPresentation.Enabled := False;
    frmPresent.KeyPreview := True;
  end;
end;

function getRepoDir(): string;
begin
  if DirectoryExists(getUserdir()+'Liederverzeichnis') then result := getUserdir()+'Liederverzeichnis'
  else if DirectoryExists(getUserdir()+'Dokumente'+ PathDelim + 'Liederverzeichnis') then result := getUserdir()+'Dokumente'+ PathDelim + 'Liederverzeichnis'
  else if DirectoryExists(ExtractFilePath(Application.ExeName) + 'Liederverzeichnis') then result := ExtractFilePath(Application.ExeName) + 'Liederverzeichnis'
  else result := '';
end;

procedure TfrmSongs.FormShow(Sender: TObject);
var filename: string;
begin
  filename := GetAppConfigFile(false);
  settings.settingsFile := TINIFile.Create(filename);
  if FileExists(filename) then
    begin
      frmSettings.loadSettings();
    end else
    begin
      ShowMessage(StrErsteBenutzung);
      frmSettings.ShowModal;
    end;
  loadRepo(frmSettings.edtRepoPath.Text);
  self.FormResize(frmSongs);
end;

procedure TfrmSongs.grbControlClick(Sender: TObject);
begin

end;

procedure TfrmSongs.grbSettingsClick(Sender: TObject);
begin

end;

procedure TfrmSongs.ImageUpdaterTimer(Sender: TObject);
begin
  ReloadPresentationImage;
  ImageUpdater.Enabled := False
end;

procedure TfrmSongs.FilterListBox(s: String);
var anz, i: integer;
begin
  lbxSRepo.Clear;
  anz := length(repo);
  if s <> '' Then
  begin
  for i := 0 to anz-1 do
    if AnsiContainsText(repo[i].Name, s) = True Then
      lbxSRepo.Items.add(repo[i].Name);
  end
  Else for i := 0 to anz-1 do
    lbxSRepo.Items.add(repo[i].Name);
end;

procedure TfrmSongs.itemEndClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmSongs.itemLoadClick(Sender: TObject);
begin
  try
    if OpenDialog.Execute then lbxSselected.Items.LoadFromFile(OpenDialog.FileName);
  except
    ShowMessage(StrFehlerOeffnen);
  end;
end;

procedure TfrmSongs.itemSaveClick(Sender: TObject);
begin
  try
    if SaveDialog.Execute then lbxSselected.Items.SaveToFile(SaveDialog.FileName);
  except
    ShowMessage(StrFehlerSpeichern);
  end;
end;

procedure TfrmSongs.lbxSRepoClick(Sender: TObject);
begin

end;

procedure TfrmSongs.lbxSRepoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Space) or (Key = VK_Return) then btnAddClick(lbxSRepo);
end;

procedure TfrmSongs.lbxSselectedClick(Sender: TObject);
begin

end;

procedure TfrmSongs.lbxSselectedDragDrop(Sender, Source: TObject; X, Y: Integer
  );
begin

end;

procedure TfrmSongs.lbxSselectedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then btnRemoveClick(lbxSSelected);
end;

procedure TfrmSongs.lbxSselectedKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TfrmSongs.lbxSselectedResize(Sender: TObject);
begin
  //edtSearch.Width:= lbxSRepo.Width;
end;

procedure TfrmSongs.FormCreate(Sender: TObject);
begin
  ProgrammMode := ModeSelection;
  self.LocaliseCaptions;
  pnlMultiScreen.Width := Round(frmSongs.Width/2);

  // Check Multiple Screen
  if Screen.MonitorCount > 1 Then
    chkMultiWindowMode.Checked := True
  Else chkMultiWindowMode.Checked := False;

  if AnsiContainsText(info.VERSION, 'beta') Then
     frmSongs.Caption := frmSongs.Caption + ' - BETA Version';
end;

procedure TfrmSongs.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Übergebe Key an Presentations-Form, wenn Vorausforderungen erfüllt sind
  if ((ProgrammMode = ModeMultiScreenPresentation) and (edtSearch.Focused = False))
  Then
  Begin
    frmPresent.FormKeyDown(frmSongs, Key, Shift);
    ImageUpdater.Enabled:=True;
  end;
end;

procedure TfrmSongs.FormKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TfrmSongs.btnAddClick(Sender: TObject);
begin
  if lbxSRepo.ItemIndex >= 0 then
    lbxSSelected.Items.Add(lbxSRepo.Items.Strings[lbxSRepo.ItemIndex]);
end;

procedure TfrmSongs.btnClearClick(Sender: TObject);
begin
  lbxSSelected.Clear;
end;

procedure TfrmSongs.btnDownClick(Sender: TObject);
var tausch: string;
begin
  if lbxSSelected.ItemIndex<lbxSSelected.Count-1 then
    begin
      tausch := lbxSselected.Items.Strings[lbxSSelected.ItemIndex+1];
      lbxSselected.Items.Strings[lbxSSelected.ItemIndex+1] :=
        lbxSselected.Items.Strings[lbxSSelected.ItemIndex];
      lbxSselected.Items.Strings[lbxSSelected.ItemIndex] := tausch;
      lbxsSelected.ItemIndex:=lbxSselected.ItemIndex+1;
    end;
end;

procedure TfrmSongs.btnGoLeftClick(Sender: TObject);
begin
  frmPresent.GoPrevious;
  //Sleep(500);
  //ReloadPresentationImage;
  ImageUpdater.Enabled := True;
end;

procedure TfrmSongs.btnGoRightClick(Sender: TObject);
begin
  frmPresent.GoNext;
  //Sleep(2000);
  //ReloadPresentationImage;
  //ReloadPresentationImage;
  ImageUpdater.Enabled := True;
end;

procedure TfrmSongs.btnQuitPresentationClick(Sender: TObject);
begin
  frmPresent.Hide;
end;

procedure TfrmSongs.btnRemoveClick(Sender: TObject);
begin
  if lbxSSelected.ItemIndex > -1 then
     lbxSSelected.Items.Delete(lbxSSelected.ItemIndex);
  if lbxSSelected.ItemIndex >= lbxSSelected.Count then
     lbxSSelected.ItemIndex := lbxSSelected.ItemIndex-1;
  lbxSSelected.SetFocus;
end;

procedure TfrmSongs.btnSettingsClick(Sender: TObject);
begin
  frmSettings.ShowModal;
  loadRepo(frmSettings.edtRepoPath.Text);
end;

procedure TfrmSongs.btnStartPresentationClick(Sender: TObject);
begin
  // Prüfe, ob mindestens ein Lied ausgewählt wurde
  if lbxSSelected.Count > 0 then
  begin
    CreatePresentationData;

    // Passe Hauptfenster an, falls Multi-Fenster-Modus ausgewählt wurde.
    if chkMultiWindowMode.Checked Then
      Begin
        ProgrammMode := ModeMultiscreenPresentation;
        ImageUpdater.Enabled:=True;
        pnlSplitter.Left := Round(frmSongs.Width / 2);
        frmSongs.FormResize(frmSongs);
        pnlMultiScreenResize(Self);
        // Falls min. zwei Bildschirme, verschiebe Präsentationsfenster auf zweite Form und starte Vollbild
        if Screen.MonitorCount > 1 Then
          begin
            Try
              frmPresent.Top := Screen.Monitors[1].Top;
              frmPresent.Left := Screen.Monitors[1].Left;
            finally
              //Falls nicht möglich, kein Problem.
            end;
          end;
        //BringToFront;
        frmSongs.KeyPreview:=True;
      end
    Else ProgrammMode := ModeSingleScreenPresentation;

    frmSongs.FormResize(frmSongs);
    // Zeige die Präsentations-Form
    frmPresent.Show();
    // Workaround für Windoof
    frmPresent.WindowState:= wsMaximized;
    if Screen.MonitorCount > 1 Then frmPresent.SwitchFullscreen(True);
    // Deaktiviere Präsentationsbutton für Zeit der Präsentation
    itemPresentation.Enabled := False;
    btnStartPresentation.Enabled := False;
    // Wurde kein Lied ausewählt, zeige eine Fehlermeldung
  end
  else ShowMessage(StrFehlerKeineLiederBeiPraesentation);
end;

procedure TfrmSongs.CreatePresentationData;
var i,j: integer;
    songfile: TStringList;
    songname: string;
    stanza: string;
begin
  present.cur:=0;
  present.textList.Clear;
  songfile := TStringList.Create();
  for i := 0 to lbxSSelected.Count-1 do
    begin
    //Ermittel Liednamen
    songname := lbxSSelected.Items.Strings[i];
    //suche Dateinamen in repo-Array
    j := 0;
    try
      while repo[j].Name <> songname do
        inc(j);
    except
      ShowMessage('Fehler: Das Lied "' + songname + '" ist nicht vorhanden. Es wird übersprungen.')
    end;
    //Lade Song-menuFile
    songfile.LoadFromFile(frmSettings.edtRepoPath.Text + PathDelim + repo[j].filePath);
    //gehe durch Songdatei und füge gleiche Strophen zu einem String zusammen
    stanza := '';
    for j := 0 to songfile.Count-1 do
    begin
      if (songfile.strings[j] = '') then
        begin
          present.textList.Add(stanza);
          present.songMetaList.Add(songname);
          stanza := '';
        end
        else stanza := stanza + songfile.Strings[j] + LineEnding;
    end;
    present.textList.Add(stanza);
    present.songMetaList.Add(songname);
    if frmSettings.cbEmptyFrame.Checked then
      begin
        present.textList.Add('');
        present.songMetaList.Add(songname);
      end;
  end;
  present.textList.Delimiter:=LineEnding;
  Clipboard.AsText := present.textList.DelimitedText;
  songfile.Free;
  end;

function TFrmSongs.GetCurrentSongPosition: TSongPosition;
var
    SongPosition: TSongPosition;
    i: integer;

begin
  SongPosition.songname:=present.songMetaList.Strings[present.cur];
  SongPosition.songposition:= 1;
  SongPosition.stanzapositionstart := 0;
  for i := 1 To present.cur do
  begin
    if present.songMetaList.Strings[i] <> present.SongMetaList.Strings[i-1] Then
      begin
      inc(SongPosition.songposition);
      SongPosition.stanzapositionstart := i;
      end;
  end;
  SongPosition.stanzaposition:=present.cur-SongPosition.stanzapositionstart+1;
  result := SongPosition;
end;

procedure TfrmSongs.UpdateSongPositionInLbxSSelected;
var
    SongPosition: TSongPosition;
begin
  SongPosition := GetCurrentSongPosition;
  lbxSselected.Itemindex := SongPosition.songposition-1;
end;

procedure TfrmSongs.btnUpClick(Sender: TObject);
var tausch: string;
begin
  if lbxSSelected.ItemIndex>0 then
    begin
      tausch := lbxSselected.Items.Strings[lbxSSelected.ItemIndex-1];
      lbxSselected.Items.Strings[lbxSSelected.ItemIndex-1] :=
        lbxSselected.Items.Strings[lbxSSelected.ItemIndex];
      lbxSselected.Items.Strings[lbxSSelected.ItemIndex] := tausch;
      lbxSselected.ItemIndex := lbxSselected.ItemIndex-1;
    end;
end;

procedure TfrmSongs.BtnUpdateClick(Sender: TObject);
begin
  ReloadPresentationImage;
end;

procedure TfrmSongs.edtSearchChange(Sender: TObject);
begin
  self.FilterListBox(edtSearch.Text);
end;

procedure TfrmSongs.BringToFront;
begin
  Application.Restore;
  Application.BringToFront;
  {$if defined(LINUX)}
    frmPresent.BringToFront;
    frmSongs.BringToFront;
  {$endif}
end;

procedure TfrmSongs.FormActivate(Sender: TObject);
begin
  if ((ProgrammMode = ModeMultiScreenPresentation) and (ProgrammMode = ModeMultiScreenPresentation)) and (frmSongs.Active <> True) and (frmPresent.Active <> True) Then
  begin
    //BringToFront;
  end;
end;

{ Diese Funktion macht ein Bildschirmfoto der Präsentation und zeigt dieses an. }

procedure TfrmSongs.ReloadPresentationImage;
var
  FormImage: TBitmap;
begin
  FormImage := frmPresent.GetFormImage;
  try
    // Clipboard.Assign(FormImage); // <=== NOPE
    imgLiveViewer.Picture.Assign(FormImage); // <-- this works ok
  finally
    FormImage.Free;
  end;
  lblFoilNumber.Caption := StrFolie + IntToStr(Present.cur + 1) + ' / ' + IntToStr(Present.TextList.Count);
  FormResize(self);
  pnlMultiScreenResize(self);
end;

end.
