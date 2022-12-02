unit songeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  editordisplaysongcontent, editorwelcome, lyrics, lclintf, ComCtrls, Menus;

type
  { TfrmSongEdit }

  TfrmSongEdit = class(TForm)
    lsSongs: TListBox;
    EditorMenu: TMainMenu;
    menuFile: TMenuItem;
    menuItemSave: TMenuItem;
    menuItemClose: TMenuItem;
    PageControl: TPageControl;
    splitter: TSplitter;
    procedure btnOpenDocsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure lsSongsClick(Sender: TObject);
    procedure menuItemCloseClick(Sender: TObject);
    procedure menuItemSaveClick(Sender: TObject);
    procedure PageControlCloseTabClicked(Sender: TObject);
  private
    repo: TRepoArray; // Load the Repo for editing it later
    //Tabs: array of TTabSheet; // Array which holds the tabs
    procedure loadRepoIntoSongListbox;
    procedure LoadSelectedSongContent;
    procedure loadFileIntoTabs(song: TRepoFile);
    procedure CreateNewTab;
    procedure CloseCurrentTab;
    procedure UpdateTabHeadlines;
    procedure OpenFileOnNewTab(song: TRepoFile);
    function CreateEditorFrame(Frame: TFrame): TfrmDisplaySongContent;
  public
    procedure loadRepo(SongRepositoryArray: TRepoArray);
  end;

var
  frmSongEdit: TfrmSongEdit;

ResourceString
  strSyntaxDocURL = 'https://www.cantara.app/tutorial/meta-data/';
  strWelcome = 'Welcome';
  strFileHasChanged = 'The file {{filename}} has been changed after opening. Would you like to save it?';

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
begin
  CloseCurrentTab;
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
  PageControl.ActivePage.Free;
  if PageControl.PageCount = 0 then CreateNewTab;
end;

procedure TfrmSongEdit.FormShow(Sender: TObject);
begin
  CreateNewTab;
end;

procedure TfrmSongEdit.btnOpenDocsClick(Sender: TObject);
begin
  OpenURL(strSyntaxDocUrl);
end;

procedure TfrmSongEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  // Update the Repository
  loadRepo(frmSettings.edtRepoPath.Text);
end;

procedure TfrmSongEdit.loadRepoIntoSongListbox;
var i: integer;
begin
  lsSongs.Items.Clear;
   for i := 0 to length(self.repo)-1 do
    lsSongs.Items.AddObject(self.repo[i].FileName, self.repo[i]);
end;

procedure TfrmSongEdit.LoadSelectedSongContent;
var selectedName: String;
  i: Integer;
begin
  selectedName := lsSongs.Items[lsSongs.ItemIndex];
  loadFileIntoTabs(lsSongs.Items.Objects[lsSongs.ItemIndex] as TRepoFile);
end;

procedure TfrmSongEdit.loadFileIntoTabs(song: TRepoFile);
var Frame: TFrame;
EditorFrame: TfrmDisplaySongContent;
begin
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

{$R *.lfm}

end.

