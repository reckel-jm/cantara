unit SongSelection;

{$mode ObjFPC}{$H+}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs, StrUtils,
  StdCtrls, ExtCtrls, Buttons, Menus, Present, settings, info, INIFiles, DefaultTranslator, Clipbrd,
  lyrics, LCLTranslator, songeditor, SongTeX;

type
  TSongPosition = record
    songname: string;
    stanzaposition: integer;
    songposition: integer;
    stanzapositionstart: integer;
  end;
  { TfrmSongs }

  TfrmSongs = class(TForm)
    btnAdd: TButton;
    btnClear: TButton;
    btnDown: TButton;
    btnQuitPresentation: TButton;
    btnRemove: TButton;
    btnGoLeft: TButton;
    btnGoRight: TButton;
    btnSettings: TButton;
    btnStartPresentation: TButton;
    btnUp: TButton;
    chkMultiWindowMode: TCheckBox;
    edtSearch: TEdit;
    grbControl: TPanel;
    grbSettings: TPanel;
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
    itemSongEditor: TMenuItem;
    itemExportTeXFile: TMenuItem;
    itemImportTeXFile: TMenuItem;
    OpenDialog: TOpenDialog;
    Control: TPanel;
    OpenSongTeXFileDialog: TOpenDialog;
    pnlMultiScreen: TPanel;
    PnlSplitter: TSplitter;
    SaveDialog: TSaveDialog;
    ImageUpdater: TTimer;
    saveSongTeXFileDialog: TSaveDialog;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grbControlClick(Sender: TObject);
    procedure grbSettingsClick(Sender: TObject);
    procedure ImageUpdaterStopTimer(Sender: TObject);
    procedure ImageUpdaterTimer(Sender: TObject);
    procedure itemEndClick(Sender: TObject);
    procedure itemExportTeXFileClick(Sender: TObject);
    procedure itemImportTeXFileClick(Sender: TObject);
    procedure itemLoadClick(Sender: TObject);
    procedure itemSaveClick(Sender: TObject);
    procedure itemSongEditorClick(Sender: TObject);
    procedure lbxSRepoClick(Sender: TObject);
    procedure lbxSRepoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbxSRepoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbxSselectedClick(Sender: TObject);
    procedure lbxSselectedDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxSselectedDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbxSselectedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbxSselectedKeyPress(Sender: TObject; var Key: char);
    procedure lbxSselectedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbxSselectedResize(Sender: TObject);
    procedure loadRepo(repoPath: string);
    procedure itemAboutClick(Sender: TObject);
    procedure itemReloadSongListClick(Sender: TObject);
    procedure pnlMultiScreenClick(Sender: TObject);
    procedure pnlMultiScreenResize(Sender: TObject);
    procedure PnlSplitterMoved(Sender: TObject);
    procedure CreatePresentationData;
    function GetCurrentSongPosition: TSongPosition;
    procedure UpdateSongPositionInLbxSSelected;
    procedure UpdateControls;
    procedure ReloadPresentationImage;
  private
    { private declarations }
    procedure LocaliseCaptions;
    procedure FilterListBox(s: String);
    procedure BringToFront;
    procedure ExportSelectionAsTeXFile;
    procedure ImportTeXFileAsSelection;
  public
    { public declarations }
    procedure AskToReloadRepo;
    function FindSong(songname: String): TRepoFile;
  end;

const
  ModeSelection = 'S';
  ModeSingleScreenPresentation = 'P';
  ModeMultiscreenPresentation = 'M';

var
  frmSongs: TfrmSongs;
  repo: TRepoArray;
  ProgramMode: char;
  startingPoint: TPoint;
  PanelMultiScreenWidth: Integer;

ResourceString
  StrErsteBenutzung = 'You are using this program for the first time. Please select a song repository folder.';
  StrFehlerOeffnen = 'Error while opening. Propably you have not the required rights to access this file.';
  StrFehlerSpeichern = 'Error while saving. Propably you have not the required rights to access this file.';
  StrFehlerKeineLiederBeiPraesentation = 'You have to add songs first.';
  StrButtonPraesentation = 'Presentation...';
  StrButtonEinstellungen = 'Settings...';
  StrCanNotOpenSong = 'Error: The Song "{songname}" is not available. Skipping.';
  StrFolie = 'Slide';
  StrFileDoesNotExist = 'The File you would like to open does not exists.';
  StrError = 'Error';

implementation

{$R *.lfm}

{ TfrmSongs }

procedure TfrmSongs.LocaliseCaptions;
begin

end;

procedure TfrmSongs.loadRepo(repoPath: string);
var SearchResult: TSearchRec;
    i,c: integer;
    songName: string;
    fileExtension: String;
begin
  if FindFirst(repoPath + PathDelim + '*', faAnyFile, SearchResult)=0 then
    begin
    lbxSRepo.Clear;
    setlength(repo, 0);
    Repeat
      // get the file extension
      fileExtension := ExtractFileExt(SearchResult.Name);
      if ((SearchResult.Name[1] <> '.') and ((fileExtension = '.song') or (fileExtension = '.txt') or (fileExtension = '.ccli'))) then  { only allow compatible file formats }
        begin
         // Finde den letzten Punkt
         songName := SearchResult.Name + '.';
         i :=-1;
         for i := 1 to length(SearchResult.Name) do
           if songName[i] = '.' then c := i;
         // Entferne die Dateiendung
         songName := copy(songName,1,c-1);
         lbxSRepo.Items.Add(songName);
         setlength(repo, length(repo)+1);
         // Füllen des Repo-Arrays zur späteren Fehlerkorrektur!
         repo[(length(repo)-1)] := TRepoFile.Create;
         repo[(length(repo)-1)].Name := songName;
         repo[(length(repo)-1)].FileName := SearchResult.Name;
         repo[(length(repo)-1)].FilePath := repoPath + PathDelim + SearchResult.Name;
         repo[(length(repo)-1)].FileExtension := fileExtension;
        end;
    Until FindNext(SearchResult)<>0;
    end;
  FindClose(SearchResult);
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

procedure TfrmSongs.PnlSplitterMoved(Sender: TObject);
begin
  grbControl.Left := PnlSplitter.Left+PnlSplitter.Width-grbControl.Width-(lbxSSelected.Width+lbxSRepo.Width) div 2;
  PanelMultiScreenWidth := frmSongs.Width-PnlSplitter.Left-1;
end;

procedure TfrmSongs.FormResize(Sender: TObject);
begin
  if (ProgramMode = ModeSelection) OR (ProgramMode = ModeSingleScreenPresentation) Then
  Begin
    {PnlSplitter.Visible := True;
    pnlSplitter.Left := frmSongs.Width-PanelMultiScreenWidth;
    pnlMultiScreen.Visible := True;
    btnStartPresentation.Enabled := False;
    frmPresent.KeyPreview := True; }

    PnlSplitter.Left := frmSongs.Width;
    PnlSplitter.Width:=1;
    IntToStr(PnlSplitter.Left);
    //PnlSplitter.Visible := False;
    pnlMultiScreen.Visible := False;
  end else
  Begin
    PnlSplitter.Visible := True;
    pnlSplitter.Left := frmSongs.Width-PanelMultiScreenWidth;
    PnlSplitter.Width := 1;
    pnlMultiScreen.Visible := True;
    itemPresentation.Enabled := False;
    btnStartPresentation.Enabled := False;
    frmPresent.KeyPreview := True;
  end;
  grbControl.Left := PnlSplitter.Left+PnlSplitter.Width-grbControl.Width-(lbxSSelected.Width+lbxSRepo.Width) div 2;
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
      frmSettings.loadSettings;
      // Maximize Window according to saved state
      if settings.settingsfile.ReadBool('Size', 'main-window-maximized', False) = True
         Then frmSongs.WindowState:= TWindowState.wsMaximized
      else frmSongs.WindowState:=TWindowState.wsNormal;
    end else
    begin
      ShowMessage(StrErsteBenutzung);
      frmSettings.ShowModal;
    end;
  loadRepo(frmSettings.edtRepoPath.Text);
  self.FormResize(frmSongs);
  frmPresent.LoadSettings; // We need to load the settings into the present form here because only at this point all the needed data is available.
  PanelMultiScreenWidth := Round(frmSongs.Width/2);
end;

procedure TfrmSongs.grbControlClick(Sender: TObject);
begin

end;

procedure TfrmSongs.grbSettingsClick(Sender: TObject);
begin

end;

procedure TfrmSongs.ImageUpdaterStopTimer(Sender: TObject);
begin
  ReloadPresentationImage;
end;

procedure TfrmSongs.ImageUpdaterTimer(Sender: TObject);
begin
  //Sleep(30);
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

procedure TfrmSongs.itemExportTeXFileClick(Sender: TObject);
begin
  ExportSelectionAsTeXFile;
end;

procedure TfrmSongs.itemImportTeXFileClick(Sender: TObject);
begin
  ImportTeXFileAsSelection;
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

procedure TfrmSongs.itemSongEditorClick(Sender: TObject);
begin
  songEditor.frmSongEdit.Show;
  songEditor.frmSongEdit.loadRepo(repo);
  //songeditor.
end;

procedure TfrmSongs.lbxSRepoClick(Sender: TObject);
begin

end;

procedure TfrmSongs.lbxSRepoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Space) or (Key = VK_Return) or (key = VK_Right) then btnAddClick(lbxSRepo);
end;

procedure TfrmSongs.lbxSRepoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    lbxSRepo.BeginDrag(False);
end;

procedure TfrmSongs.lbxSselectedClick(Sender: TObject); // Jump to the current Song – only in presentation mode
var
  selectedSongName: String;
  pos: Integer;
begin
  if ProgramMode <> ModeSelection then
  begin
    selectedSongName := lbxSSelected.Items.Strings[lbxSSelected.ItemIndex];
    pos := Present.songMetaList.IndexOf(selectedSongName);
    present.frmPresent.showItem(pos);
    if ProgramMode = ModeMultiScreenPresentation then ImageUpdater.Enabled := True; // Refresh Picture in Presentation View
  end;
end;

procedure TfrmSongs.lbxSselectedDragDrop(Sender, Source: TObject; X, Y: Integer);
var DropPosition, StartPosition: Integer;
    DropPoint: TPoint;
begin
  if (Source is TListBox) and ((Source as TListBox).Name = 'lbxSRepo') then
     lbxSSelected.Items.Add(lbxSRepo.Items.Strings[lbxSRepo.ItemIndex])
  else if (Source is TListBox) and ((Source as TListBox).Name = 'lbxSselected') then
    begin
      DropPoint.X := X;
      DropPoint.Y := Y;
      with Source as TListBox do
      begin
        StartPosition := ItemAtPos(StartingPoint,True) ;
        DropPosition := ItemAtPos(DropPoint,True) ;
        Items.Move(StartPosition, DropPosition) ;
      end;
    end;
end;

procedure TfrmSongs.lbxSselectedDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source is TListBox) then
     Accept := True;
end;

procedure TfrmSongs.lbxSselectedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ProgramMode <> ModeSelection then exit;
  if (Key = VK_DELETE) or (Key = VK_Left) then btnRemoveClick(lbxSSelected);
end;

procedure TfrmSongs.lbxSselectedKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TfrmSongs.lbxSselectedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StartingPoint.X := X;
  StartingPoint.Y := Y;
end;

procedure TfrmSongs.lbxSselectedResize(Sender: TObject);
begin
  //edtSearch.Width:= lbxSRepo.Width;
end;

procedure TfrmSongs.FormCreate(Sender: TObject);
begin
  ProgramMode := ModeSelection;
  //self.LocaliseCaptions;

  // Check Multiple Screen
  if Screen.MonitorCount > 1 Then
    chkMultiWindowMode.Checked := True
  Else chkMultiWindowMode.Checked := False;
end;

procedure TfrmSongs.FormDestroy(Sender: TObject);
var i: integer;
begin
  // Distroy all Song Data
  for i := 0 to length(repo)-1 do
    repo[i].Free;
end;

procedure TfrmSongs.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Übergebe Key an Presentations-Form, wenn Vorausforderungen erfüllt sind
  if ((ProgramMode = ModeMultiScreenPresentation) and (edtSearch.Focused = False))
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
  if (lbxSRepo.ItemIndex >= 0) and (ProgramMode = ModeSelection) then
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
        ProgramMode := ModeMultiscreenPresentation;
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
    Else ProgramMode := ModeSingleScreenPresentation;

    frmSongs.FormResize(frmSongs);
    // Zeige die Präsentations-Form
    frmPresent.Show;
    frmPresent.ShowFirst;
    // Workaround für Windoof
    frmPresent.WindowState:= wsMaximized;
    if Screen.MonitorCount > 1 Then frmPresent.SwitchFullscreen(True);
    // Deaktiviere Präsentationsbutton für Zeit der Präsentation
    itemPresentation.Enabled := False;
    btnStartPresentation.Enabled := False;
    // Lade nochmals Einstellungen (zur Sicherheit)
    // vorert nicht: frmPresent.LoadSettings;
    // Wurde kein Lied ausgewählt, zeige eine Fehlermeldung
  end
  else ShowMessage(StrFehlerKeineLiederBeiPraesentation);
  if ProgramMode = ModeMultiscreenPresentation Then begin
     ImageUpdater.Enabled:=True;
  end;
  UpdateControls;
end;

procedure TfrmSongs.UpdateControls;
begin
  if (ProgramMode = ModeMultiscreenPresentation) or (ProgramMode = ModeSingleScreenPresentation) then
  begin
    btnAdd.Enabled:=False;
    btnRemove.Enabled:=False;
    btnUp.Enabled:=False;
    btnDown.Enabled:=False;
    btnClear.Enabled:=False;
    //lbxSRepo.Enabled := False;
  end else
  begin
    btnAdd.Enabled:=True;
    btnRemove.Enabled:=True;
    btnUp.Enabled:=True;
    btnDown.Enabled:=True;
    btnClear.Enabled:=True;
    //lbxSRepo.Enabled := True;
  end;
end;

procedure TfrmSongs.CreatePresentationData;
var i,j: integer;
    songfile: TStringList;
    completefilename: String;
    songname: string;
    stanza: string;
    Song: lyrics.TSong;
    SongList: lyrics.TSongList;
begin
  present.cur:=0;
  present.textList.Clear;
  present.songMetaList.Clear;
  Songlist := lyrics.TSongList.Create;
  Songlist.FreeObjects:=False;
  present.frmPresent.Songlist := lyrics.TSongList.Create;
  for i := 0 to lbxSSelected.Count-1 do
    begin
    songfile := TStringList.Create;
    Song := lyrics.TSong.Create;
    Song.MaxSlideLineLength:=Settings.frmSettings.seWrapLines.Value;
    //Get Song Name
    songname := lbxSSelected.Items.Strings[i];
    //suche Dateinamen in repo-Array
    j := 0;
    try
      while repo[j].Name <> songname do
        inc(j);
    except
      ShowMessage(StringReplace(StrCanNotOpenSong, '{songname}', songname, [rfReplaceAll]));
    end;
    //Lade Song-menuFile abhängig von der Erweiterung!
    completefilename := frmSettings.edtRepoPath.Text + PathDelim + repo[j].FileName;
    Song.importSongfile(completefilename);
    Songlist.Add(song);
    songfile.Assign(song.output);
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
    { Add the last stanza }
    present.textList.Add(stanza);
    present.songMetaList.Add(songname);
    { Add an empty frame if selected in the settings }
    if frmSettings.cbEmptyFrame.Checked then
      begin
        present.textList.Add('');
        present.songMetaList.Add(songname);
      end;
    { Free the used Classes in the For-Loop }
    //if Assigned(Song) then Song.Free;
    if Assigned(songfile) then songfile.Free;
  end;
  // Kopiere Lieder in Zwischenablage
  if frmSettings.cbLyricsToClipboard.Checked = True Then Clipboard.AsText := lyrics.StringListToString(present.textList);
  present.frmPresent.Songlist.Assign(SongList);
  if Assigned(SongList) then SongList.Free;
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
  if ((ProgramMode = ModeMultiScreenPresentation) and (ProgramMode = ModeMultiScreenPresentation)) and (frmSongs.Active <> True) and (frmPresent.Active <> True) Then
  begin
    //BringToFront;
  end;
end;

procedure TfrmSongs.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Save WindowStates to File
  settings.settingsFile.WriteBool('Size', 'main-window-maximized',frmSongs.WindowState = TWindowState.wsMaximized);
  settings.settingsFile.UpdateFile;
  settings.settingsFile.FreeInstance;
end;

{ Diese Funktion macht ein Bildschirmfoto der Präsentation und zeigt dieses an. }

procedure TfrmSongs.ReloadPresentationImage;
var
  FormImage: TBitmap;
begin
  if ProgramMode = ModeMultiScreenPresentation then
    begin
    FormImage := frmPresent.GetFormImage;
    try
      imgLiveViewer.Picture.Assign(FormImage);
    finally
      FormImage.Free;
    end;
    lblFoilNumber.Caption := StrFolie + ' ' + IntToStr(Present.cur + 1) + ' / ' + IntToStr(Present.TextList.Count);
    FormResize(self);
    pnlMultiScreenResize(self);
  end;
end;

procedure TfrmSongs.AskToReloadRepo;
begin
  self.loadRepo(frmSettings.edtRepoPath.Text);
end;

procedure TfrmSongs.ExportSelectionAsTeXFile;
var i: integer;
  songtexfile: TSongTeXFile;
  song: TRepoFile;
  songtexFileName: String;
begin
  if saveSongTeXFileDialog.Execute = False then Exit;
  songtexfile := TSongTeXFile.Create;
  for i := 0 to lbxSselected.Count-1 do
  begin
    song := FindSong(lbxSSelected.Items.Strings[i]);
    songtexfile.AddFile(song);
  end;
  songtexFileName := saveSongTexFileDialog.FileName;
  if ExtractFileExt(songtexFileName) <> '.songtex' then
     songtexFileName := songtexFileName + '.songtex';
  songtexfile.SaveToFile(songtexFileName);
end;

function TfrmSongs.FindSong(songname: String): TRepoFile;
var i: integer;
begin
  for i := 0 to length(repo)-1 do
  begin
    if repo[i].Name = songname then
    begin
      Exit(Repo[i]);
    end
  end;
  Exit(nil);
end;

procedure TfrmSongs.ImportTeXFileAsSelection;
var SongTexFile: TSongTeXFile;
  NextFileName, RepoPath: String;
  RepoFileStrings: TStringList;
  songname, songextension: String;
  DateTimeStr: String;
begin
  if OpenSongTeXFileDialog.Execute then
  begin
    RepoPath := frmSettings.edtRepoPath.Text;
    if FileExists(OpenSongTeXFileDialog.FileName) = false then
    begin
      Application.MessageBox(PChar(strFileDoesNotExist), PChar(strError), MB_ICONWARNING or MB_OK);
    end;
    SongTeXFile := TSongTeXFile.Create;
    SongTeXFile.LoadFromFile(OpenSongTeXFileDialog.FileName);
    NextFileName := SongTeXFile.HasNextSongfile;
    while (NextFileName <> '') do
    begin
      // Check whether file with the same name exists in the repository
      if FileExists(RepoPath + PathDelim + NextFileName) then
      begin
        // Check whether files are equal
        RepoFileStrings := TStringList.Create;
        RepoFileStrings.LoadFromFile(RepoPath + PathDelim + NextFileName);
        if RepoFileStrings.Equals(SongTeXFile.NextSongFile) then
        begin
          // Add the entry from local repo
          lbxSSelected.Items.Add(Copy(NextFileName, 1, Length(NextFileName)-Length(ExtractFileExt(NextFileName))));
        end else // The song is available but not equal
        begin
          // We save the song under an imported flag and add it
          songExtension := ExtractFileExt(NextFileName);
          SongName := Copy(NextFileName, 1, Length(NextFileName)-Length(ExtractFileExt(NextFileName)));
          DateTimeToString(DateTimeStr, 'yyyy-mm-dd', Now);
          SongName := SongName + ' [' + DateTimeStr + ']';
          SongTeXFile.NextSongFile.SaveToFile(RepoPath + PathDelim + SongName + SongExtension);
          lbxSSelected.Items.Add(SongName);
          ItemReloadSongListClick(nil);
        end;
        FreeAndNil(RepoFileStrings);
      end else // The file does not exist yet, then we import the file and add it
      begin
        SongTeXFile.NextSongFile.SaveToFile(RepoPath + PathDelim + NextFileName);
        SongName := Copy(NextFileName, 1, Length(NextFileName)-Length(ExtractFileExt(NextFileName)));
        lbxSSelected.Items.Add(SongName);
        ItemReloadSongListClick(nil);
      end;
      NextFileName := SongTexFile.HasNextSongfile;
    end;
    FreeAndNil(SongTeXFile);
  end;
end;

end.
