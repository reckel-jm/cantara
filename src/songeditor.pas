unit songeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  editordisplaysongcontent, editorwelcome, lyrics, LCLType,
  lclintf, LCLTranslator, DefaultTranslator, ComCtrls, Menus, Settings;

type
  { TfrmSongEdit }

  TfrmSongEdit = class(TForm)
    lsSongs: TListBox;
    EditorMenu: TMainMenu;
    menuFile: TMenuItem;
    menuItemCopy: TMenuItem;
    menuItemClose: TMenuItem;
    menuItemArchivate: TMenuItem;
    menuItemNew: TMenuItem;
    menuItemSave: TMenuItem;
    PageControl: TPageControl;
    splitter: TSplitter;
    procedure btnOpenDocsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lsSongsClick(Sender: TObject);
    procedure menuItemCloseClick(Sender: TObject);
    procedure menuItemArchivateClick(Sender: TObject);
    procedure menuItemCopyClick(Sender: TObject);
    procedure menuItemNewClick(Sender: TObject);
    procedure menuItemSaveClick(Sender: TObject);
    procedure PageControlCloseTabClicked(Sender: TObject);
    procedure PageControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure loadRepoIntoSongListbox; // this has to be public because it will be called from outside, e.g. the frame TEditorDisplaySongContent
    procedure CloseCurrentTab;
    procedure ArchivateCurrentTab;
    procedure CopyCurrentTab(OldSongname: String);
    procedure LoadSelectedSongContent;
    procedure FindAndSelectItem(FileName: String);
  private
    repo: TRepoArray; // Load the Repo for editing it later
    procedure loadFileIntoTabs(song: TRepoFile);
    procedure CreateNewTab;
    procedure UpdateTabHeadlines;
    procedure OpenFileOnNewTab(song: TRepoFile);
    function CreateEditorFrame(Frame: TFrame): TfrmDisplaySongContent;
    function RepoContainsSongName(songname: String): Boolean;
    function CheckCanClose(Page: TTabSheet): Boolean;
  public
    procedure loadRepo(SongRepositoryArray: TRepoArray);
  end;

var
  frmSongEdit: TfrmSongEdit;
  mouseX, mouseY: Integer;

ResourceString
  strSyntaxDocURL = 'https://www.cantara.app/tutorial/meta-data/';
  strWelcome = 'Welcome';
  strFileHasChanged = 'The file {{filename}} has been changed after opening. Would you like to save it?';
  strNewFileContent = 'Please add the name of the new song';
  strNewFileCaption = 'New Song';
  StrFileNameExists = 'The name exists already. Please choose an other one.';
  StrCanNotArchivate = 'No valid file selected.';
  StrInvalidFileName = 'Invalid name. No path delimiters ("/" on Linux and "\" on Windows) are allowed.';
  StrSaveUnsavedChangesContent = 'The file {filename} has unsaved changes. Would you like to save it now?';
  StrSaveUnsavedChangesCaption = 'Question';

implementation

uses SongSelection;

procedure TfrmSongEdit.loadRepo(SongRepositoryArray: TRepoArray);
begin
  self.repo:=SongRepositoryArray;
  loadRepoIntoSongListbox;
end;

procedure TfrmSongEdit.lsSongsClick(Sender: TObject);
begin
  LoadSelectedSongContent;
end;

procedure TfrmSongEdit.menuItemCloseClick(Sender: TObject);
begin
  frmSongEdit.Close;
end;

procedure TfrmSongEdit.menuItemArchivateClick(Sender: TObject);
begin
  ArchivateCurrentTab;
end;

procedure TfrmSongEdit.menuItemCopyClick(Sender: TObject);
begin
  CopyCurrentTab('');
end;


{
This function copies the opened file (active Tab) to a new name and opens it.
}
procedure TfrmSongEdit.CopyCurrentTab(OldSongname: String);
  var
  NewSongName, NewFilePath: String;
  RepoFile: TRepoFile;
  Frame: TFrame;
  i: Integer;
begin
  NewSongName := OldSongName;
  if InputQuery(StrNewFileCaption, StrNewFileContent, NewSongName) = False then
     Exit;
  While (RepoContainsSongName(NewSongName)) do
  begin
    ShowMessage(StrFileNameExists);
    if InputQuery(StrNewFileCaption, StrNewFileContent, NewSongName) = False then
       Exit;
  end;
  While (Pos(PathDelim, NewSongName) > 0) or (length(NewSongName) < 1) do
  begin
    ShowMessage(StrInvalidFileName);
    if InputQuery(StrNewFileCaption, StrNewFileContent, NewSongName) = False then
       Exit;
  end;
  Frame := PageControl.ActivePage.FindChildControl('ContentFrame') as TFrame;
  if (Frame is TfrmDisplaySongContent) then
  begin
    RepoFile := TRepoFile.Create;
    RepoFile.Name:=NewSongName;
    RepoFile.FileExtension:='.song';
    RepoFile.FileName:=RepoFile.Name+RepoFile.FileExtension;
    NewFilePath := frmSettings.edtRepoPath.Text + PathDelim + NewSongName + '.song';
    RepoFile.FilePath:=NewFilePath;
    (Frame as TfrmDisplaySongContent).memoCode.Lines.SaveToFile(NewFilePath);
    SetLength(Repo, Length(Repo)+1);
    Repo[Length(Repo)-1] := RepoFile;
    lsSongs.AddItem(RepoFile.FileName, RepoFile);
    frmSongEdit.Repaint;
    LoadFileIntoTabs(RepoFile);
    FindAndSelectItem(RepoFile.FileName);
  end;
end;

procedure TfrmSongEdit.ArchivateCurrentTab;
  var RepoFile: TRepoFile;
    Frame: TFrame;
    NewFilePath: String;
begin
  Frame := PageControl.ActivePage.FindChildControl('ContentFrame') as TFrame;
  if (Frame is TfrmDisplaySongContent) and (FileExists((Frame as TfrmDisplaySongContent).openFile.FilePath)) then
    begin
      RepoFile := (Frame as TfrmDisplaySongContent).openFile;
      NewFilePath := frmSettings.edtRepoPath.Text + PathDelim + editordisplaysongcontent.ArchiveFolderName + PathDelim + RepoFile.FileName;
      if not RenameFile(RepoFile.FilePath, NewFilePath) then
      begin
        ShowMessage(StrCanNotArchivate);
        Exit;
      end;
      CloseCurrentTab;
      frmSongs.AskToReloadRepo;
      loadRepo(SongSelection.repo);
      LoadRepoIntoSongListbox;
      Application.ProcessMessages;
    end else
      ShowMessage(StrCanNotArchivate);
end;

procedure TfrmSongEdit.menuItemNewClick(Sender: TObject);
var
  NewSongName, NewFilePath: String;
  DummyFile: TStringList;
  RepoFile: TRepoFile;
begin
  NewSongName := '';
  if InputQuery(StrNewFileCaption, StrNewFileContent, NewSongName) = False then
     Exit;
  While (RepoContainsSongName(NewSongName)) do
  begin
    ShowMessage(StrFileNameExists);
    if InputQuery(StrNewFileCaption, StrNewFileContent, NewSongName) = False then
     Exit;
  end;
  // no path delims in song name as this will cause an exception
  While (Pos(PathDelim, NewSongName) > 0) or (length(NewSongName) < 1) do
  begin
    ShowMessage(StrInvalidFileName);
    if InputQuery(StrNewFileCaption, StrNewFileContent, NewSongName) = False then
     Exit;
  end;
  DummyFile := TStringList.Create;
  RepoFile := TRepoFile.Create;
  RepoFile.Name:=NewSongName;
  RepoFile.FileExtension:='.song';
  RepoFile.FileName:=RepoFile.Name+RepoFile.FileExtension;
  NewFilePath := frmSettings.edtRepoPath.Text + PathDelim + NewSongName + '.song';
  RepoFile.FilePath:=NewFilePath;
  DummyFile.SaveToFile(RepoFile.FilePath);
  FreeAndNil(DummyFile);
  SetLength(Repo, Length(Repo)+1);
  Repo[Length(Repo)-1] := RepoFile;
  lsSongs.AddItem(RepoFile.FileName, RepoFile);
  LoadFileIntoTabs(RepoFile);
  frmSongEdit.Repaint;
  FindAndSelectItem(RepoFile.FileName);
end;

function TfrmSongEdit.RepoContainsSongName(songname: String): Boolean;
var i: Integer;
begin
  for i := 0 to Length(Repo)-1 do
    if Repo[i].Name = songname then
      Exit(True);
  Result := False;
end;

procedure TfrmSongEdit.menuItemSaveClick(Sender: TObject);
var Frame: TFrame;
  EditFrame: TfrmDisplaySongContent;
begin
  Frame := PageControl.ActivePage.FindChildControl('ContentFrame') as TFrame;
  if (Frame.ClassType = TfrmDisplaySongContent) then
    begin
      EditFrame := Frame as TfrmDisplaySongContent;
      EditFrame.SaveFile;
    end;
end;

procedure TfrmSongEdit.PageControlCloseTabClicked(Sender: TObject);
var i: Integer;
begin
  i := PageControl.IndexOfPageAt(mouseX, mouseY);
  if (i > -1) and (CheckCanClose(PageControl.Pages[i])) then
  begin
    PageControl.Pages[i].Free;
    if PageControl.PageCount = 0 then CreateNewTab;
  end;
end;

procedure TfrmSongEdit.PageControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  mouseX := X;
  mouseY := Y;
end;

function TfrmSongEdit.CheckCanClose(Page: TTabSheet): Boolean;
var MessageBoxReturn: Integer;
  Frame: TFrame;
  MessageBoxString: String;
begin
  Result := True;
  Frame := Page.FindChildControl('ContentFrame') as TFrame;
  if Frame.ClassType = TfrmDisplaySongContent then
  begin
    // Check and ask to save unsaved changes
    if (Frame as TfrmDisplaySongContent).hasChanged then
    begin
      MessageBoxString := StringReplace(StrSaveUnsavedChangesContent, '{filename}', (Frame as TfrmDisplaySongContent).openFile.FileName, [rfReplaceAll]);
      MessageBoxReturn := Application.MessageBox(PChar(MessageBoxString), PChar(StrSaveUnsavedChangesCaption), MB_ICONQUESTION+MB_YESNOCANCEL);
      if MessageBoxReturn = IDYES then
        menuItemSaveClick(frmSongEdit) // TODO: We should add an extra Procedure here later.
      else if MessageBoxReturn = IDCANCEL then
        Result := False; // Abour the closure process
    end;
  end;
end;

procedure TfrmSongEdit.CreateNewTab;
var ContentFrame: TFrame;
  NewTab: TTabSheet;
begin
  NewTab := TTabSheet.Create(PageControl);
  NewTab.PageControl := PageControl;
  NewTab.Name := 'Tab' + IntToStr(PageControl.PageCount);
  NewTab.Caption := strWelcome;
  ContentFrame := TfrmEditorWelcome.Create(NewTab);
  ContentFrame.Name := 'ContentFrame';
  ContentFrame.Align := TAlign.alClient;
  ContentFrame.Parent := NewTab;
end;

procedure TfrmSongEdit.CloseCurrentTab;
begin
  if CheckCanClose(PageControl.ActivePage) then
  begin
    PageControl.ActivePage.Free;
    if PageControl.PageCount = 0 then CreateNewTab;
  end;
end;

procedure TfrmSongEdit.FormShow(Sender: TObject);
begin
  if PageControl.PageCount = 0 then CreateNewTab;
  splitter.Left := SettingsFile.ReadInteger('Size', 'editor-splitter-location', 500);
end;

procedure TfrmSongEdit.btnOpenDocsClick(Sender: TObject);
begin
  OpenURL(strSyntaxDocUrl);
end;

procedure TfrmSongEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  // Update the Repository
  frmSongs.AskToReloadRepo;
  SettingsFile.WriteInteger('Size', 'editor-splitter-location', splitter.Left);
end;

procedure TfrmSongEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
   Page: TTabSheet;
begin
{ We check whether any of the tabs which has unsaved changes should be saved and close
them one by one. }
while (0 <= PageControl.PageCount-1) do
  begin
    Page := PageControl.Pages[0];
    if CheckCanClose(Page) then
    begin
      Page.Free;
      Continue;
    end else
    begin
      CanClose := False;
      Exit;
    end;
  end;
end;

procedure TfrmSongEdit.FormCreate(Sender: TObject);
begin

end;

procedure TfrmSongEdit.FormDblClick(Sender: TObject);
begin

end;

procedure TfrmSongEdit.loadRepoIntoSongListbox;
var i: integer;
begin
  lsSongs.Items.Clear;
   for i := 0 to length(self.repo)-1 do
    lsSongs.Items.AddObject(self.repo[i].FileName, self.repo[i]);
end;

procedure TfrmSongEdit.LoadSelectedSongContent;
begin
  loadFileIntoTabs(lsSongs.Items.Objects[lsSongs.ItemIndex] as TRepoFile);
end;

{ This Procedure will open a song in a tab. If the current tab has no unsaved changes, it will be opened at its place.
If the file in the current tab has unsaved changes, it will open a new tab. }
procedure TfrmSongEdit.loadFileIntoTabs(song: TRepoFile);
var Frame: TFrame;
EditorFrame: TfrmDisplaySongContent;
i: Integer;
begin
  for i := 0 to PageControl.PageCount-1 do
  begin
    Frame := PageControl.Pages[i].FindChildControl('ContentFrame') as TFrame;
    if Frame.ClassType = TfrmDisplaySongContent then
      if (Frame as TfrmDisplaySongContent).openFile.FilePath = song.FilePath then
      begin
        PageControl.PageIndex:=i; // switch to the open tab if file is already open
        Exit;
      end;
  end;
  Frame := PageControl.ActivePage.FindChildControl('ContentFrame') as TFrame;
  if Frame.ClassType = TfrmEditorWelcome then
  begin
    Frame.Free;
    Frame := TfrmDisplaySongContent.Create(PageControl.ActivePage);
    EditorFrame := CreateEditorFrame(Frame);
    EditorFrame.loadFile(song);
  end else if (Frame.ClassType = TfrmDisplaySongContent) and ((Frame as TfrmDisplaySongContent).hasChanged = False) then
  begin
    EditorFrame := Frame as TfrmDisplaySongContent;
    EditorFrame.loadFile(song);
  end else if (Frame.ClassType = TfrmDisplaySongContent) and ((Frame as TfrmDisplaySongContent).hasChanged = True) then
    OpenFileOnNewTab(song);
  PageControl.ActivePage.Caption:=song.Name;
end;

function TfrmSongEdit.CreateEditorFrame(Frame: TFrame): TfrmDisplaySongContent;
begin
  Frame.Name := 'ContentFrame';
  Frame.Parent:=PageControl.ActivePage;
  Frame.Align := TAlign.alClient;
  //Frame.OnFileChanged := @UpdateTabHeadlines;
  Result := Frame as TfrmDisplaySongContent;
end;

procedure TfrmSongEdit.OpenFileOnNewTab(song: TRepoFile);
var Frame: TFrame;
EditorFrame: TfrmDisplaySongContent;
begin
  CreateNewTab;
  PageControl.ActivePageIndex:=PageControl.PageCount-1;
  Frame := PageControl.ActivePage.FindChildControl('ContentFrame') as TFrame;
  Frame.Free;
  Frame := TfrmDisplaySongContent.Create(PageControl.ActivePage);
  EditorFrame := CreateEditorFrame(Frame);
  EditorFrame.loadFile(song);
end;

procedure TfrmSongEdit.UpdateTabHeadlines;
begin

end;

procedure TfrmSongEdit.FindAndSelectItem(FileName: String);
var i: Integer;
begin
  for i := 0 to lsSongs.Count-1 do
  begin
    if lsSongs.Items[i] = FileName then
      begin
        lsSongs.ItemIndex:=i;
        Break;
      end;
  end;
end;

{$R *.lfm}

end.

