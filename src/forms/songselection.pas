unit SongSelection;

{$mode ObjFPC}{$H+}

interface

uses
  LCLType, LCLIntf, Classes, SysUtils, FileUtil, LazFileUtils, RTTICtrls, Forms,
  Controls, Graphics, Dialogs, StrUtils, Math,
  StdCtrls, ExtCtrls, Buttons, Menus, Present, settings, info, INIFiles,
  DefaultTranslator, Clipbrd,
  lyrics, LCLTranslator, songeditor, SongTeX, welcome, Slides,
  FormFulltextSearch, PPTX, PresentationCanvas, PresentationModels,
  formMarkupExport, imageexport, textfilehandler, CantaraStandardDialogs,
  presentationcontroller, Types, bgrabitmap, BGRABitmapTypes, FlatpakHandling,
  FormSongStyle, SelectionJSON
  ;

type
  TProgramMode = (ModeSelection, ModeSingleScreenPresentation,
  ModeMultiscreenPresentation);

  TSongPosition = record
    // The Current Song
    song: TSong;
    // The current song name
    songname: String;
    stanzaposition: Integer;
    songposition: Integer;
    stanzapositionstart: Integer;
  end;
  { TfrmSongs }

  { The main form of Cantara where the songs are choosen from.
  It is also responsible for managing the song repository }
  TfrmSongs = class(TForm, IPresentationController)
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
    ButtonCloseSongtexFile: TButton;
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
    itemSeperator1: TMenuItem;
    itemEnd: TMenuItem;
    menuEdit: TMenuItem;
    itemSettings: TMenuItem;
    menuHelp: TMenuItem;
    itemAbout: TMenuItem;
    itemPresentation: TMenuItem;
    itemReloadSongList: TMenuItem;
    itemSongEditor: TMenuItem;
    itemShowWelcomeAssistent: TMenuItem;
    itemOpenInEditor: TMenuItem;
    itemFulltextSearch: TMenuItem;
    menuExport: TMenuItem;
    itemLoadSelection: TMenuItem;
    itemSaveSelection: TMenuItem;
    itemSaveSelectionAs: TMenuItem;
    itemExportPptx: TMenuItem;
    itemMarkupExport: TMenuItem;
    itemExportPictures: TMenuItem;
    itemSelectAllSongs: TMenuItem;
    OpenDialog: TOpenDialog;
    ControlPanel: TPanel;
    PanelSongTeXStatus: TPanel;
    pnlMultiScreen: TPanel;
    PnlSplitter: TSplitter;
    Separator1: TMenuItem;
    SlideTextListBox: TListBox;
    itemEditSongStyle: TMenuItem;
    SongPopupMenu: TPopupMenu;
    SaveDialog: TSaveDialog;
    SplitterContentImage: TSplitter;
    TimerUpdateScreen: TTimer;

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
    procedure ButtonCloseSongtexFileClick(Sender: TObject);
    procedure chkMultiWindowModeChange(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure itemEndClick(Sender: TObject);
    procedure itemExportPicturesClick(Sender: TObject);
    procedure itemExportPptxClick(Sender: TObject);
    procedure itemFulltextSearchClick(Sender: TObject);
    procedure itemLoadClick(Sender: TObject);
    procedure itemLoadSelectionClick(Sender: TObject);
    procedure itemMarkupExportClick(Sender: TObject);
    procedure itemOpenInEditorClick(Sender: TObject);
    procedure itemSaveClick(Sender: TObject);
    procedure itemSaveSelectionAsClick(Sender: TObject);
    procedure itemSaveSelectionClick(Sender: TObject);
    procedure itemSelectAllSongsClick(Sender: TObject);
    procedure itemShowWelcomeAssistentClick(Sender: TObject);
    procedure itemSongEditorClick(Sender: TObject);
    procedure lbxSRepoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbxSRepoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbxSselectedClick(Sender: TObject);
    procedure lbxSselectedDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxSselectedDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbxSselectedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbxSselectedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbxSselectedResize(Sender: TObject);
    { loadRepo oads the Song Repository from the repository folder, creates an TSongFile
    class instance for each file and adds it to the repo array.
    @param(repoPath is the complete absolute file path to the song repepository without a
    path delim ('/') or ('\') at its end. }
    procedure loadRepo(repoPath: String);
    procedure itemAboutClick(Sender: TObject);
    procedure itemReloadSongListClick(Sender: TObject);
    procedure pnlMultiScreenResize(Sender: TObject);
    { Creates the song list data }
    procedure CreateSongListData;
    { Opens the selected songs and creates the presentation data from the selected songs. }
    procedure CreateSongListDataAndLoadItIntoSlideList(ASlideList: TSlideList);
    function GetCurrentSongPosition: TSongPosition;
    procedure SlideTextListBoxClick(Sender: TObject);
    procedure SlideTextListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure SlideTextListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SlideTextListBoxMeasureItem(Control: TWinControl; Index: Integer;
      var AHeight: Integer);
    procedure itemEditSongStyleClick(Sender: TObject);
    procedure SongPopupMenuPopup(Sender: TObject);
    procedure TimerUpdateScreenTimer(Sender: TObject);
    { Updates the Song Position in lbxSSelected during a presentation }
    procedure UpdateSongPositionInLbxSSelected;
    { Updates the Controls to the current program state (Presentation, Songselection }
    procedure UpdateControls;
    procedure ReloadPresentationImage;
    procedure PresentationHasBeenEnded;
  private
    {
      This boolean determines whether the chkMultiWindowMode checkbox has been
      changed by the user so that the automatic detection will get inactive.
    }
    MultiScreenCheckBoxHasBeenManuallyChanged: Boolean;
    { Per-song custom style overrides (key=FilePath, Objects=TCustomSongStyleEntry) }
    FCustomSongStyles: TStringList;
    { The initial position of the panel for multiscreen
      when presentation gets started }
    PanelMultiScreenLeft: Integer;
    { The loaded song selection file path }
    LoadedSongSelectionFilePath: String;
    { This integer counts for the presentation slide id's }
    PresentationSlideCounter: Integer;
    { Songlist which conatains the loaded songs }
    LoadedSongList: TSongList;
    { User agrees to use PPTXGenjs }
    UserAgreesPptxGenJs: Boolean;
    PreviouslyMultiScreen: Boolean;
    { Filters the Listbox lbxSRepo after a search pattern. If s is empty, no filter will be applied.
    @param(s: the search pattern) }
    procedure FilterListBox(s: String);
    { Per-song style helpers }
    function  GetCustomStyleForSong(const FilePath: String;
      out AStyle: TPresentationStyleSettings): Boolean;
    procedure SetCustomStyleForSong(const FilePath: String;
      const AStyle: TPresentationStyleSettings);
    procedure RemoveCustomStyleForSong(const FilePath: String);
    function  FindFileInRepoRecursive(const RepoPath, Filename: String): String;
    procedure BringToFront;
    procedure ExportSelectionAsTeXFile(var FilePath: String);
    function ImportSongTeXFileAsSelection(FilePath: String): Boolean;
    procedure ExportSelectionAsJsonFile(const FilePath: String);
    function  ImportSelectionFromJsonFile(const FilePath: String): Boolean;
    { Saves the Selection at the place FilePath
      @param(FilePath: The full File Path where to save the song selection)
    }
    procedure SaveSelection(var FilePath: String);
    procedure LoadSongTeXFile(FilePath: String);
    { Loads the Slide Texts in a List View }
    procedure FillSlideListInPresentationConsole;
    { Returns whether a second screen is usuable or not }
    function MultiScreenIsUsable: Boolean;
  public
    { public declarations }

    {
     The mode in which cantara is at the moment (Song selection, single screen
     presentation or multi screen presentation
    }
    ProgramMode: TProgramMode;

    procedure AskToReloadRepo;
    function FindSong(songname: String): TRepoFile;
  end;

var
  frmSongs: TfrmSongs;
  { The Repository array which contains songs as classes of TSongFile }
  repo: TRepoArray;
  startingPoint: TPoint;

resourcestring
  StrErrorOpening =
    'Error while opening. Propably you have not the required rights to access this file.';
  StrErrorSaving =
    'Error while saving. Propably you have not the required rights to access this file.';
  StrFehlerKeineLiederBeiPraesentation = 'You have to add songs first.';
  StrButtonPraesentation = 'Presentation...';
  StrButtonEinstellungen = 'Settings...';
  StrCanNotOpenSong = 'Error: The Song "{songname}" is not available. Skipping.';
  StrFolie = 'Slide';
  StrFileDoesNotExist = 'The File you would like to open does not exists.';
  StrError = 'Error';
  StrHint = 'Hint';
  StrActiveSongTeXFile = 'The following file is opened at the moment: ';
  StrSongTeXFileSongsImported =
    'The songs from the file have been imported to your song repository.';
  StrPptxGenjs =
    'Cantara is using your local default web browser''s Java Script engine and the open source Java Script library PptxGenJs (https://gitbrent.github.io/PptxGenJS/) to generate the pptx file. Therefore, after pressing OK, your web browser will open and ask you for a place to save the pptx file when the generation was done succesfully. During this process, no internet connection will be needed and no data is shared with anyone else despite your web browser. If you want to continue, press OK and this message won''t show up again next time. If you don''t want to continue, please press Cancel.';
  StrSongIsEmpty = 'The song {songname} is empty. It will not be added.';
  StrTitleSlide = 'Title Slide';

implementation

{$R *.lfm}

type
  TCustomSongStyleEntry = class
    PresentationStyleSettings: TPresentationStyleSettings;
    destructor Destroy; override;
  end;

destructor TCustomSongStyleEntry.Destroy;
begin
  DestroyPresentationStyleSettings(PresentationStyleSettings);
  inherited;
end;

{ TfrmSongs }

procedure TfrmSongs.loadRepo(repoPath: String);
var
  SongPath: String;
  songName: String;
  i: Integer;
  SongInList: Boolean;
  song: TRepoFile;
  FindAllFilesSearchResult: TStringList;
  Pathlist: Array of String;
begin
  { Delete everything in repo if there is something.
    Here we need to be careful and check that the song has not been selected already.
    If the song is selected already, we won't free the TRepoFile-Object to avoid nil pointer exceptions.
  }
  for song In repo do
  begin
    SongInList := False;
    for i := 0 to lbxSSelected.Count-1 do
    begin
      if TRepoFile(lbxSSelected.Items.Objects[i]).FilePath = song.FilePath then
         SongInList := True;
    end;
    if not SongInList then song.Free;
  end;

  SetLength(repo, 0);
  FindAllFilesSearchResult := FindAllFiles(repoPath, '*.txt;*.song;*.ccli', True);

  if FindAllFilesSearchResult.Count > 0 then
  begin
    lbxSRepo.Clear;
    setlength(repo, 0);
    for SongPath in FindAllFilesSearchResult do
    begin
      { Check whether song is in archive -> then ignore it and don't import it }
      Pathlist := SongPath.Split(PathDelim);
      if (Length(Pathlist) > 2) and ((Pathlist[Length(Pathlist)-2]) = 'archive') then
         Continue;

      songName := ExtractFileNameOnly(SongPath);
      setlength(repo, length(repo) + 1);

      { Fill the record array with the data }
      repo[(length(repo) - 1)] := TRepoFile.Create;
      repo[(length(repo) - 1)].Name := songName;
      repo[(length(repo) - 1)].FileName := ExtractFileName(SongPath);
      repo[(length(repo) - 1)].FilePath := SongPath;
      repo[(length(repo) - 1)].FileExtension := ExtractFileExt(SongPath);

      lbxSRepo.Items.AddObject(songName, repo[(length(repo) - 1)]);
    end;
  end;

  FindAllFilesSearchResult.Free;
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

procedure TfrmSongs.pnlMultiScreenResize(Sender: TObject);
begin
  imgLiveViewer.Width := pnlMultiScreen.Width;
  // Falls Bild noch nicht da, nehme FrmPresent Höhe
  if imgLiveViewer.Height < 10 then
    imgLiveViewer.Height := frmPresent.Height
  // Passe Höhe an Bild an
  else if (imgLiveViewer.Picture.Bitmap.Width > 0) then
    imgLiveViewer.Height := round(imgLiveViewer.Width *
      imgLiveViewer.Picture.Bitmap.Height / imgLiveViewer.Picture.Bitmap.Width);
end;

procedure TfrmSongs.FormResize(Sender: TObject);
begin
  if (ProgramMode = ModeSelection) Or (ProgramMode = ModeSingleScreenPresentation) then
  begin
    PnlSplitter.Left := frmSongs.Width;
    PnlSplitter.Width := 1;
    IntToStr(PnlSplitter.Left);
    pnlMultiScreen.Visible := False;
    grbControl.Width := 45;
    grbControl.Left := PnlSplitter.Left + PnlSplitter.Width - grbControl.Width -
    (lbxSSelected.Width + lbxSRepo.Width) Div 2;
  end
  else
  begin
    PnlSplitter.Visible := True;
    pnlSplitter.Left := frmSongs.Width - PanelMultiScreenLeft;
    PnlSplitter.Width := 1;
    pnlMultiScreen.Visible := True;
    itemPresentation.Enabled := False;
    btnStartPresentation.Enabled := False;
    frmPresent.KeyPreview := True;
    grbControl.Left := 0;
    grbControl.Width := 0;
  end;
end;

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

procedure TfrmSongs.FormShow(Sender: TObject);
var
  filename, openfilepath: String;
begin
  filename := GetAppConfigFile(False);
  settings.settingsFile := TINIFile.Create(filename);
  frmSettings.loadSettings;
  if FileExists(filename) then
  begin
    // Maximize Window according to saved state
    if settings.settingsfile.ReadBool('Size', 'main-window-maximized', False) =
      True then frmSongs.WindowState := TWindowState.wsMaximized
    else
      frmSongs.WindowState := TWindowState.wsNormal;
  end
  else
  begin
    frmWelcome.ShowModal;
    settingsfile.UpdateFile;
  end;
  self.UserAgreesPptxGenJs := SettingsFile.ReadBool('Exporter', 'pptxgenjs', False);
  loadRepo(frmSettings.edtRepoPath.Text);
  self.FormResize(frmSongs);
  PanelMultiScreenLeft := Round(frmSongs.Width / 2);
  PanelMultiScreenLeft := Max(Trunc(0.8 * frmSongs.Width),
    settingsfile.ReadInteger('Size', 'panel-mutliscreen-position',
    PanelMultiScreenLeft));
  // Load Song File if the program reveived one
  try
    if ParamCount = 1 then
    begin
      OpenFilePath := ParamStr(1);
      if FileExists(OpenFilePath) And (ExtractFileExt(OpenFilePath) = '.songtex') then
        LoadSongTeXFile(OpenFilePath);
    end;
  finally
  end;

  {$if defined(Container)}
    if (lbxSRepo.Count = 0) and (not FlatpakHandling.CheckPortalUsed(frmSettings)) then
    begin
      ShowMessage(flatpakhandling.MessageCantaraNeedsPortalUse);
      frmSettings.btnSelectDirClick(frmSongs);
      Self.AskToReloadRepo;
    end;
  {$endif}
end;

procedure TfrmSongs.FilterListBox(s: String);
var
  anz, i: Integer;
begin
  lbxSRepo.Clear;
  anz := length(repo);
  if s <> '' then
  begin
    for i := 0 to anz - 1 do
      if AnsiContainsText(repo[i].Name, s) = True then
        lbxSRepo.Items.add(repo[i].Name);
  end
  else
    for i := 0 to anz - 1 do
      lbxSRepo.Items.add(repo[i].Name);
end;

procedure TfrmSongs.itemEndClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmSongs.itemExportPicturesClick(Sender: TObject);
begin
  if lbxSSelected.Count > 0 then
  begin
    CreateSongListDataAndLoadItIntoSlideList(FormImageExport.SlideList);
    FormImageExport.PresentationCanvas.PresentationStyleSettings :=
      frmSettings.ExportPresentationStyleSettings;
    FormImageExport.PresentationCanvas.SlideSettings :=
      frmSettings.ExportSlideSettings();

    FormImageExport.RunExporter(Self.Programmode > TProgramMode.ModeSelection);
  end
  else
    Application.MessageBox(PChar(StrFehlerKeineLiederBeiPraesentation),
      PChar(StrError), MB_OK + MB_ICONWARNING);
end;

procedure TfrmSongs.itemExportPptxClick(Sender: TObject);
var
  PPTXExporter: TPPTXExporter;
  PPTXSlideList: TSlideList;
  {$IF not defined(CONTAINER)}
  TempDir: String;
  {$ENDIF}
begin
  if lbxSSelected.Count > 0 then
  begin
    if (Not self.UserAgreesPptxGenJs) And
      (Application.MessageBox(PChar(StrPptxGenjs), PChar(StrHint),
      MB_OKCANCEL + MB_ICONINFORMATION) <> 1) then Exit;
    self.UserAgreesPptxGenJs := True;
    PPTXExporter := TPPTXExporter.Create;
    PPTXExporter.PresentationStyleSettings :=
      frmSettings.ExportPresentationStyleSettings;
    {$IF defined(CONTAINER)}
    PPTXExporter.Folder := frmSettings.edtRepoPath.Text + PathDelim + 'exports';
    if not DirectoryExists(PPTXExporter.Folder) then
      if not CreateDir(PPTXExporter.Folder) then Exit;
    {$ELSE}
    TempDir := GetTempDir(False); // get the users temp dir
    PPTXEXporter.Folder := TempDir;
    {$ENDIF}
    if Self.ProgramMode = TProgramMode.ModeSelection then
    begin
      PPTXSlideList := TSlideList.Create(True);
      CreateSongListDataAndLoadItIntoSlideList(PPTXSlideList);
    end
    else
    begin
      // In Presentation Mode, don't destroy the slides as they are owned by the
      // presentation window
      PPTXSlideList := TSlideList.Create(False);
      PPTXSlideList.Assign(frmPresent.SlideList);
    end;

    PPTXExporter.AddSlides(PPTXSlideList);
    OpenURL('file://' + PPTXExporter.SaveJavaScriptToFile);
    PPTXSlideList.Destroy;
    PPTXExporter.Destroy;
  end
  else
    Application.MessageBox(PChar(StrFehlerKeineLiederBeiPraesentation),
      PChar(StrError), MB_OK + MB_ICONWARNING);
end;

procedure TfrmSongs.itemFulltextSearchClick(Sender: TObject);
begin
  frmWrapperFulltextSearch.ShowModal(self);
  // We overload this function so that the child know the parent and it's size
  Application.ProcessMessages;
  frmWrapperFulltextSearch.Invalidate;
  frmWrapperFulltextSearch.Repaint;
end;

procedure TfrmSongs.itemLoadClick(Sender: TObject);
begin
  try
    if ExecuteCantaraFileDialog(OpenDialog) then lbxSselected.Items.LoadFromFile(OpenDialog.FileName);
  except
    Application.MessageBox(PChar(StrErrorOpening), PChar(StrError),
      MB_OK + MB_ICONERROR);
  end;
end;

procedure TfrmSongs.itemLoadSelectionClick(Sender: TObject);
var
  OpenFilePath: String;
begin
  if ExecuteCantaraFileDialog(OpenDialog) then OpenFilePath := OpenDialog.FileName
  else
    Exit;
  if FileExists(OpenFilePath) = False then
  begin
    Application.MessageBox(PChar(strFileDoesNotExist), PChar(strError),
      MB_ICONWARNING Or MB_OK);
    Exit;
  end;
  LoadSongTeXFile(OpenFilePath);
end;

procedure TfrmSongs.LoadSongTeXFile(FilePath: String);
var
  SongTexFileIsSelection: Boolean;
begin
  SongTexFileIsSelection := True;
  lbxSSelected.Clear;
  if ExtractFileExt(FilePath) = '.songtex' then
    SongTexFileIsSelection := ImportSongTeXFileAsSelection(FilePath)
  else if ExtractFileExt(FilePath) = '.json' then
    ImportSelectionFromJsonFile(FilePath)
  else if ExtractFileExt(FilePath) = '.csswc' then
  begin
    { This is really depreciated and should not be used anymore... }
    lbxSselected.Items.LoadFromFile(OpenDialog.FileName);
  end
  else
    Exit;
  if SongTexFileIsSelection then
  begin
    self.LoadedSongSelectionFilePath := FilePath;
    PanelSongTeXStatus.Visible := True;
    PanelSongTeXStatus.Height := EdtSearch.Height;
    PanelSongTeXStatus.Caption := StrActiveSongTeXFile + LoadedSongSelectionFilePath;
  end
  else
    Application.MessageBox(PChar(StrSongTeXFileSongsImported), PChar(StrHint),
      MB_ICONINFORMATION);
end;

procedure TfrmSongs.FillSlideListInPresentationConsole;
var
  Slide: TSlide;
  AddedText: String;
begin
  SlideTextListBox.Clear;
  for Slide In frmPresent.SlideList do
  begin
    AddedText := Trim(Slide.PartContent.MainText);
    if Slide.SlideType = SlideTypeEnum.TitleSlide then
      AddedText := AddedText + ' (' + StrTitleSlide + ')';
    SlideTextListBox.Items.Add(AddedText);
  end;
end;

function TfrmSongs.MultiScreenIsUsable: Boolean;
begin
  // Update the Screens
  Screen.UpdateMonitors;
  Result := Screen.MonitorCount > 1;
end;

procedure TfrmSongs.itemMarkupExportClick(Sender: TObject);
begin
  if lbxSSelected.Count > 0 then
  begin
    if Self.ProgramMode = TProgramMode.ModeSelection then CreateSongListData;
    FrmMarkupExport.Show;
    FrmMarkupExport.SongList := LoadedSongList;
    FrmMarkupExport.ParseTemplate;
  end
  else
    Application.MessageBox(PChar(StrFehlerKeineLiederBeiPraesentation),
      PChar(StrError), MB_OK + MB_ICONWARNING);
end;

procedure TfrmSongs.itemOpenInEditorClick(Sender: TObject);
var
  repoFile: TRepoFile;
  i: Integer;
  CallingListbox: TListBox;
begin
  if SongPopUpMenu.PopupComponent = lbxSRepo then
    CallingListbox := lbxSRepo
  else if SongPopUpMenu.PopupComponent = lbxSSelected then
    CallingListbox := lbxSSelected
  else
    Exit;
  if (CallingListbox.ItemIndex >= 0) then
  begin
    frmSongEdit.Show;
    frmSongEdit.loadRepo(repo);
    Application.ProcessMessages;
    for i := 0 to length(repo) - 1 do
    begin
      if repo[i].Name = CallingListbox.Items[CallingListbox.ItemIndex] then
      begin
        repoFile := repo[i];
        Break;
      end;
    end;
    for i := 0 to frmSongEdit.lsSongs.Count - 1 do
    begin
      if frmSongEdit.lsSongs.Items[i] = repoFile.FileName then
      begin
        try
          frmSongEdit.lsSongs.ItemIndex := i;
          Application.ProcessMessages;
          frmSongEdit.Repaint;
          frmSongEdit.lsSongsClick(frmSongs);
        finally
        end;
        Break;
      end;
    end;
  end;
end;

procedure TfrmSongs.itemSaveClick(Sender: TObject);
begin
  try
    if ExecuteCantaraFileDialog(SaveDialog) then lbxSselected.Items.SaveToFile(SaveDialog.FileName);
  except
    Application.MessageBox(PChar(StrErrorSaving), PChar(StrError), MB_OK + MB_ICONERROR);
  end;
end;

procedure TfrmSongs.itemSaveSelectionAsClick(Sender: TObject);
var FileName: String;
begin
  if ExecuteCantaraFileDialog(SaveDialog) then
  begin
    FileName := SaveDialog.FileName;
    if ExtractFileExt(FileName) = '' then
    begin
      if SaveDialog.FilterIndex = 2 then
        FileName := FileName + '.json'
      else
        FileName := FileName + '.songtex';
    end;
    SaveSelection(FileName);
    LoadedSongSelectionFilePath := FileName;
    PanelSongTeXStatus.Visible := True;
    PanelSongTeXStatus.Height := EdtSearch.Height;
    PanelSongTeXStatus.Caption := StrActiveSongTeXFile + LoadedSongSelectionFilePath;
  end;
end;

procedure TfrmSongs.itemSaveSelectionClick(Sender: TObject);
begin
  if LoadedSongSelectionFilePath = '' then itemSaveSelectionAsClick(itemSaveSelection)
  else
    SaveSelection(LoadedSongSelectionFilePath);
end;

procedure TfrmSongs.itemSelectAllSongsClick(Sender: TObject);
var i: Integer;
begin
  for i := 0 to lbxSRepo.Count-1 do
    lbxSSelected.Items.Add(lbxSRepo.Items.Strings[i]);
end;

procedure TfrmSongs.SaveSelection(var FilePath: String);
var TextFileHandler: TTextFileHandler;
begin
  TextFileHandler := TTextFileHandler.Create;
  if ExtractFileExt(FilePath) = '.songtex' then
    ExportSelectionAsTeXFile(FilePath)
  else if ExtractFileExt(FilePath) = '.json' then
    ExportSelectionAsJsonFile(FilePath)
  else if ExtractFileExt(FilePath) = '.csswc' then
  begin
    { This is really depreciated and should not be used anymore... }
    lbxSselected.Items.SaveToFile(FilePath);
  end;
  TextFileHandler.Destroy;
end;

procedure TfrmSongs.itemShowWelcomeAssistentClick(Sender: TObject);
begin
  frmWelcome.ShowModal;
end;

procedure TfrmSongs.itemSongEditorClick(Sender: TObject);
begin
  frmSongEdit.Show;
  frmSongEdit.loadRepo(repo);
end;

procedure TfrmSongs.lbxSRepoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ProgramMode <> ModeSelection then
  begin
    Key := VK_Unknown;
    Exit;
  end;
  if (Key = VK_Space) Or (Key = VK_Return) Or (key = VK_Right) then
    btnAddClick(lbxSRepo);
  if (Key = VK_Tab) then
  begin
    lbxSSelected.SetFocus;
    Key := VK_Unknown;
    if lbxSSelected.Count > 0 then
       lbxSSelected.ItemIndex := Max(lbxSSelected.ItemIndex, 0);
  end;
end;

procedure TfrmSongs.lbxSRepoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    lbxSRepo.BeginDrag(False);
end;

procedure TfrmSongs.lbxSselectedClick(Sender: TObject);
var
  i, pos, count: Integer;
begin
  if lbxSSelected.ItemIndex < 0 then Exit;
  // Jump to the current Song – only in presentation mode
  if ProgramMode <> ModeSelection then
  begin
    try
      pos := lbxSSelected.ItemIndex;
      count := 0;
      if (pos = 0) and (frmPresent.SlideList.Count > 0) then
        frmPresent.ShowFirst
      else for i := 1 to frmPresent.SlideList.Count - 1 do
      begin
        if frmPresent.SlideList.Items[i].Song <>
          frmPresent.SlideList.Items[i-1].Song then
        begin
          count := count + 1;
          if count = pos then
          begin
            frmPresent.showItem(i);
            Break;
          end;
        end;
      end;
    finally
      // Sometimes there is an error here
    end;
  end;
end;

procedure TfrmSongs.lbxSselectedDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DropPosition, StartPosition: Integer;
  DropPoint: TPoint;
begin
  If lbxSSelected.Count <= 0 then Exit;
  If Source = nil then Exit;
  If Sender = nil then Exit;
  try
    if lbxSRepo.ItemIndex < 0 then Exit;
    if (Source Is TListBox) And ((Source As TListBox).Name = 'lbxSRepo') then
      lbxSSelected.Items.Add(lbxSRepo.Items.Strings[lbxSRepo.ItemIndex])
    else if (Source Is TListBox) And ((Source As TListBox).Name = 'lbxSselected') then
    begin
      DropPoint.X := X;
      DropPoint.Y := Y;
      with Source As TListBox do
      begin
        StartPosition := ItemAtPos(StartingPoint, True);
        DropPosition := ItemAtPos(DropPoint, True);
        if (StartPosition = -1) or (DropPosition = -1) then Exit;
        Items.Move(StartPosition, DropPosition);
      end;
    end;
  finally
  end;
end;

procedure TfrmSongs.lbxSselectedDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source Is TListBox) then
    Accept := True;
end;

procedure TfrmSongs.lbxSselectedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ProgramMode <> ModeSelection then
  begin
    Key := VK_Unknown;
    Exit;
  end;
  if (Key = VK_DELETE) Or (Key = VK_Left) then btnRemoveClick(lbxSSelected)
  else if (Key = VK_Tab) then
  begin
    lbxSRepo.SetFocus;
    Key := VK_Unknown;
  end;
end;

procedure TfrmSongs.lbxSselectedMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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

  LoadedSongSelectionFilePath := '';
  // we hide the panel
  PanelSongTeXStatus.Visible := False;
  PanelSongTeXStatus.Height := 0;
  PanelSongTeXStatus.Caption := '';

  Self.LoadedSongList := TSongList.Create;
  FCustomSongStyles := TStringList.Create;

  // load home directory into file/folder dialogs
  OpenDialog.InitialDir := GetUserDir;
  SaveDialog.InitialDir := GetUserDir;

  Self.MultiScreenCheckBoxHasBeenManuallyChanged := False;
  Self.PreviouslyMultiScreen := False;
end;

procedure TfrmSongs.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  // Distroy all Song Data
  for i := 0 to length(repo) - 1 do
    repo[i].Free;
  try
     LoadedSongList.Destroy;
  finally
  end;
  for i := 0 to FCustomSongStyles.Count - 1 do
    FCustomSongStyles.Objects[i].Free;
  FCustomSongStyles.Free;
end;

procedure TfrmSongs.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  try
    LoadSongTeXFile(FileNames[0]);
  finally
  end;
end;

procedure TfrmSongs.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //Übergebe Key an Presentations-Form, wenn Vorausforderungen erfüllt sind
  if ((ProgramMode = ModeMultiScreenPresentation) And (edtSearch.Focused = False)) then
  begin
    frmPresent.FormKeyDown(frmSongs, Key, Shift);
  end;
end;

procedure TfrmSongs.btnAddClick(Sender: TObject);
begin
  if (lbxSRepo.ItemIndex >= 0) And (ProgramMode = ModeSelection) then
  begin
    lbxSSelected.Items.AddObject(lbxSRepo.Items.Strings[lbxSRepo.ItemIndex], lbxSRepo.Items.Objects[lbxSRepo.ItemIndex]);
  end;
end;

procedure TfrmSongs.btnClearClick(Sender: TObject);
begin
  lbxSSelected.Clear;
end;

procedure TfrmSongs.btnDownClick(Sender: TObject);
begin
  if lbxSSelected.ItemIndex < lbxSSelected.Count - 1 then
  begin
    lbxSSelected.Items.Exchange(lbxSSelected.ItemIndex, lbxSSelected.ItemIndex + 1);
    lbxSSelected.ItemIndex := lbxSSelected.ItemIndex + 1;
  end;
end;

procedure TfrmSongs.btnGoLeftClick(Sender: TObject);
begin
  frmPresent.GoPrevious;
end;

procedure TfrmSongs.btnGoRightClick(Sender: TObject);
begin
  frmPresent.GoNext;
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
    lbxSSelected.ItemIndex := lbxSSelected.ItemIndex - 1;
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
    PresentationSlideCounter := 0;
    CreateSongListDataAndLoadItIntoSlideList(frmPresent.SlideList);
    if frmPresent.SlideList.Count <= 0 then Exit;
    frmSongs.FormResize(frmSongs);
    // Take the settings from the Settings Form
    frmPresent.PresentationCanvas.PresentationStyleSettings :=
      frmSettings.ExportPresentationStyleSettings;
    frmPresent.PresentationCanvas.SlideSettings := frmSettings.ExportSlideSettings();
    // Workaround für Windoof
    frmPresent.WindowState := wsMaximized;
    // Zeige die Präsentations-Form
    frmPresent.Show;
    frmPresent.PresentationCanvas.Width := frmPresent.Width;
    frmPresent.PresentationCanvas.Height := frmPresent.Height;
    frmPresent.PresentationCanvas.LoadBackgroundBitmap;
    frmPresent.Invalidate;
    frmPresent.ShowFirst;
    // Deaktiviere Präsentationsbutton für Zeit der Präsentation
    itemPresentation.Enabled := False;
    btnStartPresentation.Enabled := False;
    // Passe Hauptfenster an, falls Multi-Fenster-Modus ausgewählt wurde.
    if chkMultiWindowMode.Checked then
    begin
      ProgramMode := ModeMultiscreenPresentation;
      pnlSplitter.Left := Round(frmSongs.Width * 0.3);
      frmSongs.FormResize(frmSongs);
      pnlMultiScreenResize(Self);
      // Falls min. zwei Bildschirme, verschiebe Präsentationsfenster auf zweite Form und starte Vollbild
      if Self.MultiScreenIsUsable then
      begin
        Screen.UpdateMonitors; // Just to go sure
        if Screen.MonitorCount > 1 then
        begin
          // Disable full screen
          frmPresent.SwitchFullScreen(False);
          Application.ProcessMessages;
          // Move the form to the second
          frmPresent.Top := Screen.Monitors[1].Top;
          frmPresent.Left := Screen.Monitors[1].Left;
          Application.ProcessMessages;
          // Full screen has to be applied after positioning, else the form won't be moved under Linux
          frmPresent.SwitchFullscreen(True);
          {$IF defined(WINDOWS) }
          frmPresent.Top := Screen.Monitors[1].Top;
          frmPresent.Left := Screen.Monitors[1].Left;
          frmPresent.Width :=Screen.Monitors[1].Width;
          frmPresent.Height :=Screen.Monitors[1].Height;
          Application.ProcessMessages;
          {$ENDIF }
        end;
      end;
      //BringToFront;
      self.KeyPreview := True;
      self.UpdateSongPositionInLbxSSelected;
      self.SlideTextListBox.SetFocus;
    end
    else
    begin
      ProgramMode := ModeSingleScreenPresentation;
      // We make sure that the Presentation window will be shown at the same screen as the frmSongs.
      frmPresent.Top := Screen.Monitors[0].Top+(Screen.Monitors[0].Height-frmPresent.Height) div 2;
      frmPresent.Left := Screen.Monitors[0].Left+(Screen.Monitors[0].Width-frmPresent.Width) div 2;
    end;
  end
  else // Wurde kein Lied ausgewählt, zeige eine Fehlermeldung
    Application.MessageBox(PChar(StrFehlerKeineLiederBeiPraesentation),
      PChar(StrError), MB_OK + MB_ICONWARNING);
  if ProgramMode = ModeMultiscreenPresentation then
  begin
    Invalidate;
    FillSlideListInPresentationConsole;
    PnlSplitter.Left := Round(Self.Width * 0.3);
    if SlideTextListBox.Count > 0 then
    begin
       SlideTextListBox.ItemIndex:=0;
       ReloadPresentationImage;
       Application.ProcessMessages;
    end;
    Application.ProcessMessages;
  end;
  UpdateControls;
end;

procedure TfrmSongs.UpdateControls;
begin
  if (ProgramMode = ModeMultiscreenPresentation) Or
    (ProgramMode = ModeSingleScreenPresentation) then
  begin
    btnAdd.Enabled := False;
    btnRemove.Enabled := False;
    btnUp.Enabled := False;
    btnDown.Enabled := False;
    btnClear.Enabled := False;
    if (ProgramMode = TProgramMode.ModeMultiscreenPresentation) then
    begin
      lbxSRepo.Width := 0;
      grbControl.Width := 0;
    end
  end else if ProgramMode = TProgramMode.ModeSelection then
  begin
    SlideTextListBox.Clear;
    btnAdd.Enabled := True;
    btnRemove.Enabled := True;
    btnUp.Enabled := True;
    btnDown.Enabled := True;
    btnClear.Enabled := True;
    lbxSRepo.Visible := True;
    grbControl.Visible := True;
  end;
end;

procedure TfrmSongs.CreateSongListData;
var
  i: Integer;
  completefilename: String;
  songname: String;
  Song: lyrics.TSong;
  SongList: lyrics.TSongList;
begin
  SongList := LoadedSongList;
  SongList.Clear;

  for i := 0 to lbxSSelected.Count - 1 do
  begin
    Song := lyrics.TSong.Create;
    //Get Song Name
    songname := TRepoFile(lbxSSelected.Items.Objects[i]).Name;
    // Lade Songfile abhängig von der Erweiterung!
    completefilename := TRepoFile(lbxSSelected.Items.Objects[i]).FilePath;
    Song.importSongfile(completefilename);
    if Song.IsEmpty then
      Application.MessageBox(PChar(StringReplace(StrSongIsEmpty,
        '{songname}', songname, [rfReplaceAll])), PChar(StrError), MB_OK + MB_ICONERROR)
    else
      Songlist.Add(song);
  end;
end;

procedure TfrmSongs.CreateSongListDataAndLoadItIntoSlideList(ASlideList: TSlideList);
var
  Song: TSong;
  SongSlideList: TSlideList;
  HasOverride: Boolean;
  OverrideStyle, ManualStyle: TPresentationStyleSettings;
  BgFilename, BgFilePath: String;
  NewFont: TFont;
  k, SlidesAddedBefore: Integer;
begin
  if Self.ProgramMode = ModeSelection then
  begin
    CreateSongListData;
    if not Assigned(ASlideList) then
      ASlideList := TSlideList.Create(True);
    ASlideList.Clear;
    if LoadedSongList.Count <= 0 then Exit; // Prevent loading an empty Presentation
  end;

  for Song In LoadedSongList do
  begin
    SongSlideList := CreatePresentationDataFromSong(Song,
      frmSettings.ExportSlideSettings(), PresentationSlideCounter);
    SlidesAddedBefore := ASlideList.Count;
    ASlideList.AddList(SongSlideList);

    // Determine per-song style override
    HasOverride := False;

    // Auto: background-image metadata tag
    if Song.MetaDict.IndexOf('background-image') >= 0 then
    begin
      BgFilename := Trim(Song.MetaDict['background-image']);
      BgFilePath := FindFileInRepoRecursive(frmSettings.edtRepoPath.Text, BgFilename);
      if BgFilePath <> '' then
      begin
        OverrideStyle := frmSettings.ExportPresentationStyleSettings;
        OverrideStyle.ShowBackgroundImage := True;
        OverrideStyle.BackgroundImageFilePath := BgFilePath;
        HasOverride := True;
      end;
    end;

    // Manual override wins over auto
    if GetCustomStyleForSong(Song.CompleteFilePath, ManualStyle) then
    begin
      if HasOverride then DestroyPresentationStyleSettings(OverrideStyle);
      OverrideStyle := ManualStyle;
      HasOverride := True;
    end;

    // Stamp all slides of this song with the override
    if HasOverride then
    begin
      for k := SlidesAddedBefore to ASlideList.Count - 1 do
      begin
        ASlideList.Items[k].HasCustomStyle := True;
        ASlideList.Items[k].CustomStyle := OverrideStyle;
        // Each slide needs its own TFont instance (avoid double-free on destroy)
        NewFont := TFont.Create;
        NewFont.Assign(OverrideStyle.Font);
        ASlideList.Items[k].CustomStyle.Font := NewFont;
      end;
      DestroyPresentationStyleSettings(OverrideStyle);
    end;

    SongSlideList.Destroy;
  end;
end;

function TfrmSongs.GetCurrentSongPosition: TSongPosition;
var
  SongPosition: TSongPosition;
  i: Integer;
begin
  SongPosition.song := frmPresent.SlideList.Items[frmPresent.cur].Song;
  SongPosition.songname := SongPosition.song.FileNameWithoutEnding;
  SongPosition.songposition := 1;
  SongPosition.stanzapositionstart := 0;
  for i := 1 to frmPresent.cur do
  begin
    if frmPresent.SlideList.Items[i].Song <>
      frmPresent.SlideList.Items[i - 1].Song then
    begin
      Inc(SongPosition.songposition);
      SongPosition.stanzapositionstart := i;
    end;
  end;
  SongPosition.stanzaposition := frmPresent.cur - SongPosition.stanzapositionstart + 1;
  Result := SongPosition;
end;

procedure TfrmSongs.SlideTextListBoxClick(Sender: TObject);
begin
  // We display the selected part but make it very defensefly
  if (SlideTextListBox.ItemIndex > -1) And (SlideTextListBox.ItemIndex <
    SlideTextListBox.Count) then
    if SlideTextListBox.ItemIndex < frmPresent.SlideList.Count then
    begin
      frmPresent.showItem(SlideTextListBox.ItemIndex);
    end;
end;

procedure TfrmSongs.SlideTextListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  TextColor: TColor;
  ABitmap:TBgraBitmap;
  {Determines whether the part is at the edge (the first or last part of a song }
  Edge: Boolean;
  DisplayedText: String;
begin
  Edge := False;
  if Index > SlideTextListBox.Count then Exit;

  ABitmap := TBgraBitmap.Create(ARect.Width, ARect.Height, clAppWorkspace);

  ABitmap.CanvasBGRA.TextStyle.SingleLine:=False;
  ABitmap.CanvasBGRA.TextStyle.Wordbreak:=True;
  ABitmap.CanvasBGRA.TextStyle.Opaque:=False;
  ABitmap.FontAntialias:=False;

  ABitmap.FontHeight:=Round(Screen.SystemFont.Height/0.75);
  ABitmap.FontName:=Screen.SystemFont.Name;
  ABitmap.FontQuality:=fqFineAntialiasing;
  DisplayedText := PresentationCanvas.GetWordWrappedString(
                SlideTextListBox.Items[Index],
                ABitmap.FontName,
                ABitmap.FontHeight,
                ABitmap.FontStyle,
                ABitmap.Width
                );
  if odSelected in State then
  begin
    ABitmap.FillRect(1,1,ABitmap.Width-1, ABitmap.Height-1, clHighlight, dmSet);
    TextColor := clHighlightText;
  end
  else
  begin
    TextColor := clBtnText;
  end;

  if (Index = 0) or
            (frmPresent.SlideList.Items[Index].Song <> frmPresent.SlideList.Items[Index-1].Song) then
  begin
    ABitmap.DrawLine(1,1,ARect.Width,1,clBlack,true);
    Edge := True;
  end;

  if frmPresent.SlideList.Items[Index].SlideType = TitleSlide then
  begin
     ABitmap.FontStyle+=[fsBold];
  end;
  ABitmap.TextRect(Rect(5,5,ABitmap.Width-5, ABitmap.Height-5),
                    DisplayedText, taLeftJustify, tlTop,
                    ColorToRGB(TextColor)
                    );
  try
    { This savely checks if we are on the last slide -> Edge }
    if (Index >= frmPresent.SlideList.Count-1) or
       (frmPresent.SlideList[Index].Song.FileNameWithoutEnding <>
       frmPresent.SlideList[Index+1].Song.FileNameWithoutEnding)
    then
    begin
      Edge := True;
      ABitmap.DrawLine(1,ARect.Height-1,ARect.Width-1,ARect.Height-1,clBlack,true);

      ABitmap.DrawLine(1,0,1,ARect.Height-1,clBlack,True);
      ABitmap.DrawLine(ARect.Width-1,0,ARect.Width-1,ARect.Height-1,clBlack,True);
    end;

    if not Edge then
    begin
      ABitmap.DrawLine(1,0,1,ARect.Height,clBlack,True);
      ABitmap.DrawLine(ARect.Width-1,0,ARect.Width-1,ARect.Height,clBlack,True);
    end else
    begin
      ABitmap.DrawLine(1,1,1,ARect.Height-1,clBlack,True);
      ABitmap.DrawLine(ARect.Width-1,1,ARect.Width-1,ARect.Height-1,clBlack,True);
    end;

    ABitmap.Draw(SlideTextListBox.Canvas, ARect.Left, ARect.Top, true);
  finally
    ABitmap.Destroy;
  end;
end;

procedure TfrmSongs.SlideTextListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Key := VK_Unknown; // We don't want to handle any keys by the List Box.
end;

procedure TfrmSongs.SlideTextListBoxMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
var
  ABitmap:TBgraBitmap;
  WrappedText: String;
begin
  if Index > SlideTextListBox.Count-1 then Exit;

  ABitmap := TBgraBitmap.Create;
  ABitmap.FontName:=Screen.SystemFont.Name;
  ABitmap.FontHeight:=Round(Screen.SystemFont.Height/0.75);
  ABitmap.CanvasBGRA.TextStyle.SingleLine:=False;
  ABitmap.CanvasBGRA.TextStyle.Wordbreak:=True;
  ABitmap.SetSize(SlideTextListBox.Width, Self.Height);
  WrappedText := PresentationCanvas.GetWordWrappedString(
                SlideTextListBox.Items[Index],
                ABitmap.FontName,
                ABitmap.FontHeight,
                ABitmap.FontStyle,
                ABitmap.Width
                );
  AHeight:=ABitmap.TextSize(WrappedText,SlideTextListBox.Width).Height;
  AHeight += 10;
  ABitmap.Destroy;
end;

function TfrmSongs.FindFileInRepoRecursive(const RepoPath, Filename: String): String;
var
  Found: TStringList;
begin
  Result := '';
  Found := FindAllFiles(RepoPath, Filename, True);
  try
    if Found.Count > 0 then
      Result := Found[0];
  finally
    Found.Free;
  end;
end;

function TfrmSongs.GetCustomStyleForSong(const FilePath: String;
  out AStyle: TPresentationStyleSettings): Boolean;
var
  Idx: Integer;
  Entry: TCustomSongStyleEntry;
begin
  Idx := FCustomSongStyles.IndexOf(FilePath);
  if Idx < 0 then
  begin
    Result := False;
    Exit;
  end;
  Entry := TCustomSongStyleEntry(FCustomSongStyles.Objects[Idx]);
  // Return a deep copy so caller owns the Font
  AStyle := Entry.PresentationStyleSettings;
  AStyle.Font := TFont.Create;
  AStyle.Font.Assign(Entry.PresentationStyleSettings.Font);
  Result := True;
end;

procedure TfrmSongs.SetCustomStyleForSong(const FilePath: String;
  const AStyle: TPresentationStyleSettings);
var
  Idx: Integer;
  Entry: TCustomSongStyleEntry;
begin
  Idx := FCustomSongStyles.IndexOf(FilePath);
  if Idx >= 0 then
  begin
    // Replace existing entry
    FCustomSongStyles.Objects[Idx].Free;
    Entry := TCustomSongStyleEntry.Create;
    Entry.PresentationStyleSettings := AStyle;
    Entry.PresentationStyleSettings.Font := TFont.Create;
    Entry.PresentationStyleSettings.Font.Assign(AStyle.Font);
    FCustomSongStyles.Objects[Idx] := Entry;
  end
  else
  begin
    Entry := TCustomSongStyleEntry.Create;
    Entry.PresentationStyleSettings := AStyle;
    Entry.PresentationStyleSettings.Font := TFont.Create;
    Entry.PresentationStyleSettings.Font.Assign(AStyle.Font);
    FCustomSongStyles.AddObject(FilePath, Entry);
  end;
end;

procedure TfrmSongs.RemoveCustomStyleForSong(const FilePath: String);
var
  Idx: Integer;
begin
  Idx := FCustomSongStyles.IndexOf(FilePath);
  if Idx >= 0 then
  begin
    FCustomSongStyles.Objects[Idx].Free;
    FCustomSongStyles.Delete(Idx);
  end;
end;

procedure TfrmSongs.itemEditSongStyleClick(Sender: TObject);
var
  RepoFile: TRepoFile;
  ExistingStyle, GlobalStyle, NewStyle: TPresentationStyleSettings;
  HasExisting: Boolean;
begin
  if lbxSSelected.ItemIndex < 0 then Exit;
  RepoFile := TRepoFile(lbxSSelected.Items.Objects[lbxSSelected.ItemIndex]);
  if RepoFile = nil then Exit;

  HasExisting := GetCustomStyleForSong(RepoFile.FilePath, ExistingStyle);
  GlobalStyle := frmSettings.ExportPresentationStyleSettings;
  try
    if HasExisting then
    begin
      frmSongStyle.LoadFromStyle(ExistingStyle, True);
      DestroyPresentationStyleSettings(ExistingStyle);
    end
    else
      frmSongStyle.LoadFromStyle(GlobalStyle, False);
  finally
    DestroyPresentationStyleSettings(GlobalStyle);
  end;

  if frmSongStyle.ShowModal = mrOK then
  begin
    if frmSongStyle.ResetToDefault then
      RemoveCustomStyleForSong(RepoFile.FilePath)
    else
    begin
      NewStyle := frmSongStyle.ExportStyle;
      SetCustomStyleForSong(RepoFile.FilePath, NewStyle);
      DestroyPresentationStyleSettings(NewStyle);
    end;
  end;
end;

procedure TfrmSongs.SongPopupMenuPopup(Sender: TObject);
begin
  itemOpenInEditor.Visible := (lbxSRepo.ItemIndex >= 0);
  itemEditSongStyle.Visible :=
    (SongPopupMenu.PopupComponent = lbxSSelected) and
    (lbxSSelected.ItemIndex >= 0);
end;

procedure TfrmSongs.TimerUpdateScreenTimer(Sender: TObject);
var MultiScreen: Boolean;
begin
  MultiScreen := Self.MultiScreenIsUsable;
  if (self.PreviouslyMultiScreen <> MultiScreen) then // There has been a change in the amount of screens (e.g. a monitor has been plugged in/out in the mean time)
  begin
    Self.chkMultiWindowMode.Checked := MultiScreen;
    MultiScreenCheckBoxHasBeenManuallyChanged := False
  end;
  self.PreviouslyMultiScreen:=MultiScreen;
end;

procedure TfrmSongs.UpdateSongPositionInLbxSSelected;
var
  SongPosition: TSongPosition;
begin
  SongPosition := GetCurrentSongPosition;
  lbxSselected.ItemIndex := SongPosition.songposition - 1;
end;

procedure TfrmSongs.btnUpClick(Sender: TObject);
begin
  if lbxSSelected.ItemIndex > 0 then
  begin
    lbxSselected.Items.Exchange(lbxSSelected.ItemIndex, lbxSSelected.ItemIndex - 1);
    lbxSSelected.ItemIndex := lbxSSelected.ItemIndex - 1;
  end;
end;

procedure TfrmSongs.ButtonCloseSongtexFileClick(Sender: TObject);
begin
  LoadedSongSelectionFilePath := '';
  // we hide the panel
  PanelSongTeXStatus.Visible := False;
  PanelSongTeXStatus.Height := 0;
  PanelSongTeXStatus.Caption := '';
end;

procedure TfrmSongs.chkMultiWindowModeChange(Sender: TObject);
begin
  Self.MultiScreenCheckBoxHasBeenManuallyChanged := True;
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
  if ((ProgramMode = ModeMultiScreenPresentation) And
    (ProgramMode = ModeMultiScreenPresentation)) And (frmSongs.Active <> True) And
    (frmPresent.Active <> True) then
  begin
    //BringToFront;
  end;
end;

procedure TfrmSongs.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Save WindowStates to File
  settings.settingsFile.WriteBool('Size', 'main-window-maximized',
    frmSongs.WindowState = wsMaximized);
  settings.settingsfile.WriteInteger('Size', 'panel-mutliscreen-position',
    PanelMultiScreenLeft);
  settings.settingsFile.WriteBool('Exporter', 'pptxgenjs', self.UserAgreesPptxGenJs);
  settings.settingsFile.UpdateFile;
end;

{ Diese Funktion macht ein Bildschirmfoto der Präsentation und zeigt dieses an. }

procedure TfrmSongs.ReloadPresentationImage;
begin
  if ProgramMode = ModeMultiScreenPresentation then
  begin
    try
      imgLiveViewer.Picture.Bitmap.Assign(frmPresent.imageShower.Picture.Bitmap);
      if SlideTextListBox.Count > frmPresent.cur then
        SlideTextListBox.ItemIndex := frmPresent.cur;
      self.UpdateSongPositionInLbxSSelected;
    finally
    end;
    lblFoilNumber.Caption := StrFolie + ' ' + IntToStr(frmPresent.cur + 1) +
      ' / ' + IntToStr(frmPresent.SlideList.Count);
    self.pnlMultiScreenResize(self);
  end;
end;

procedure TfrmSongs.PresentationHasBeenEnded;
begin
  Self.ProgramMode := SongSelection.ModeSelection;
  Self.UpdateControls;
end;

procedure TfrmSongs.AskToReloadRepo;
begin
  self.loadRepo(frmSettings.edtRepoPath.Text);
end;

procedure TfrmSongs.ExportSelectionAsTeXFile(var FilePath: String);
var
  i: Integer;
  songtexfile: TSongTeXFile;
  song: TRepoFile;
  songtexfilename: String;
  TextFileHandler: TTextFileHandler;
  TextFile: String;
begin
  TextFileHandler := TTextFileHandler.Create;
  songtexfile := TSongTeXFile.Create;
  for i := 0 to lbxSselected.Count - 1 do
  begin
    song := FindSong(lbxSSelected.Items.Strings[i]);
    songtexfile.AddFile(song);
  end;
  songtexFileName := FilePath;
  if ExtractFileExt(Filepath) <> '.songtex' then
    songtexFileName := songtexFileName + '.songtex';
  TextFile := songtexfile.Text;
  TextFileHandler.SaveTextFile(TextFile, FilePath, '.songtex');
  TextFileHandler.Destroy;
end;

function TfrmSongs.FindSong(songname: String): TRepoFile;
var
  i: Integer;
begin
  for i := 0 to length(repo) - 1 do
  begin
    if repo[i].Name = songname then
    begin
      Exit(Repo[i]);
    end;
  end;
  Exit(nil);
end;

function TfrmSongs.ImportSongTeXFileAsSelection(FilePath: String): Boolean;
var
  SongTexFile: TSongTeXFile;
  NextFileName, RepoPath: String;
  RepoFileStrings: TStringList;
  songname, songextension: String;
  DateTimeStr: String;
begin
  RepoPath := frmSettings.edtRepoPath.Text;
  SongTeXFile := TSongTeXFile.Create;
  SongTeXFile.LoadFromFile(FilePath);
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
        if SongTexFile.SongTeXIsSelection then
          lbxSSelected.Items.Add(Copy(NextFileName, 1,
            Length(NextFileName) - Length(ExtractFileExt(NextFileName))));
      end
      else // The song is available but not equal
      begin
        // We save the song under an imported flag and add it
        songExtension := ExtractFileExt(NextFileName);
        SongName := Copy(NextFileName, 1, Length(NextFileName) -
          Length(ExtractFileExt(NextFileName)));
        DateTimeToString(DateTimeStr, 'yyyy-mm-dd', Now);
        SongName := SongName + ' [' + DateTimeStr + ']';
        SongTeXFile.NextSongFile.SaveToFile(RepoPath + PathDelim +
          SongName + SongExtension);
        if SongTexFile.SongTeXIsSelection then
          lbxSSelected.Items.Add(SongName);
        ItemReloadSongListClick(nil);
      end;
      FreeAndNil(RepoFileStrings);
    end
    else // The file does not exist yet, then we import the file and add it
    begin
      SongTeXFile.NextSongFile.SaveToFile(RepoPath + PathDelim + NextFileName);
      SongName := Copy(NextFileName, 1, Length(NextFileName) - Length(
        ExtractFileExt(NextFileName)));
      if SongTexFile.SongTeXIsSelection then
        lbxSSelected.Items.Add(SongName);
      ItemReloadSongListClick(nil);
    end;
    NextFileName := SongTexFile.HasNextSongfile;
  end;
  Result := SongTexFile.SongTeXIsSelection;
  SongTexFile.Destroy;
end;

procedure TfrmSongs.ExportSelectionAsJsonFile(const FilePath: String);
var
  JSONFile: TSelectionJSONFile;
  i: Integer;
  SongFile: TRepoFile;
  HasStyle: Boolean;
  Style: TPresentationStyleSettings;
begin
  JSONFile := TSelectionJSONFile.Create;
  try
    for i := 0 to lbxSSelected.Count - 1 do
    begin
      SongFile := FindSong(lbxSSelected.Items.Strings[i]);
      if SongFile = nil then Continue;
      HasStyle := GetCustomStyleForSong(SongFile.FilePath, Style);
      try
        JSONFile.AddSong(SongFile.FilePath, SongFile.FileName, HasStyle, Style);
      finally
        if HasStyle then DestroyPresentationStyleSettings(Style);
      end;
    end;
    JSONFile.SaveToFile(FilePath);
  finally
    JSONFile.Free;
  end;
end;

function TfrmSongs.ImportSelectionFromJsonFile(const FilePath: String): Boolean;
var
  JSONFile: TSelectionJSONFile;
  i: Integer;
  RepoPath, FinalFileName, FullPath, BgPath: String;
  SongExt, SongNameBase, DateTimeStr: String;
  ExistingContent, NewContentSL: TStringList;
  Style: TPresentationStyleSettings;
  FS: TFileStream;
  SongRepoFile: TRepoFile;
begin
  Result := True; // JSON selections are always a selection, never an archive
  RepoPath := frmSettings.edtRepoPath.Text;

  JSONFile := TSelectionJSONFile.Create;
  try
    JSONFile.LoadFromFile(FilePath);

    for i := 0 to High(JSONFile.Songs) do
    begin
      FinalFileName := JSONFile.Songs[i].FileName;
      FullPath := RepoPath + PathDelim + FinalFileName;

      { --- Song file: check repo and save if needed --- }
      if FileExists(FullPath) then
      begin
        ExistingContent := TStringList.Create;
        NewContentSL    := TStringList.Create;
        try
          ExistingContent.LoadFromFile(FullPath);
          NewContentSL.Text := JSONFile.Songs[i].FileContent;
          if not ExistingContent.Equals(NewContentSL) then
          begin
            // Different content — save under a [YYYY-MM-DD] suffix
            SongExt      := ExtractFileExt(FinalFileName);
            SongNameBase := Copy(FinalFileName, 1,
              Length(FinalFileName) - Length(SongExt));
            DateTimeToString(DateTimeStr, 'yyyy-mm-dd', Now);
            FinalFileName := SongNameBase + ' [' + DateTimeStr + ']' + SongExt;
            FullPath := RepoPath + PathDelim + FinalFileName;
            FS := TFileStream.Create(FullPath, fmCreate);
            try
              if Length(JSONFile.Songs[i].FileContent) > 0 then
                FS.Write(JSONFile.Songs[i].FileContent[1],
                  Length(JSONFile.Songs[i].FileContent));
            finally
              FS.Free;
            end;
            ItemReloadSongListClick(nil);
          end;
          // else: file exists and is identical — use it as-is
        finally
          FreeAndNil(ExistingContent);
          FreeAndNil(NewContentSL);
        end;
      end
      else
      begin
        // File does not exist — save it
        FS := TFileStream.Create(FullPath, fmCreate);
        try
          if Length(JSONFile.Songs[i].FileContent) > 0 then
            FS.Write(JSONFile.Songs[i].FileContent[1],
              Length(JSONFile.Songs[i].FileContent));
        finally
          FS.Free;
        end;
        ItemReloadSongListClick(nil);
      end;

      // Add the (possibly renamed) song name to the selection.
      // FindSong returns the TRepoFile from the repo array (populated by
      // loadRepo/ItemReloadSongListClick). AddObject is essential — bare
      // Items.Add leaves a nil object pointer that crashes CreateSongListData.
      SongNameBase := Copy(FinalFileName, 1,
        Length(FinalFileName) - Length(ExtractFileExt(FinalFileName)));
      SongRepoFile := FindSong(SongNameBase);
      lbxSSelected.Items.AddObject(SongNameBase, SongRepoFile);

      { --- Custom style: restore if present --- }
      if JSONFile.Songs[i].Style.HasCustomStyle then
      begin
        // Save background image to repo dir if data is present
        BgPath := '';
        if JSONFile.Songs[i].BackgroundImageData <> '' then
        begin
          BgPath := RepoPath + PathDelim +
            JSONFile.Songs[i].Style.BackgroundImageFilename;
          if not FileExists(BgPath) then
          begin
            FS := TFileStream.Create(BgPath, fmCreate);
            try
              FS.Write(JSONFile.Songs[i].BackgroundImageData[1],
                Length(JSONFile.Songs[i].BackgroundImageData));
            finally
              FS.Free;
            end;
          end;
        end;

        Style := JSONFile.SongStyleToPresSettings(i);
        try
          Style.BackgroundImageFilePath := BgPath;
          SetCustomStyleForSong(FullPath, Style);
        finally
          DestroyPresentationStyleSettings(Style);
        end;
      end;
    end;
  finally
    JSONFile.Free;
  end;
end;

end.
