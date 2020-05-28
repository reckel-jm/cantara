unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, Present, settings, info, INIFiles;

type

  { TfrmSongs }

  TfrmSongs = class(TForm)
    btnAdd: TButton;
    btnRemove: TButton;
    btnUp: TButton;
    btnDown: TButton;
    btnStartPresentation: TButton;
    btnSettings: TButton;
    btnClear: TButton;
    grbSettings: TGroupBox;
    grbControl: TGroupBox;
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
    MenuItem1: TMenuItem;
    itemPresentation: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnStartPresentationClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grbControlClick(Sender: TObject);
    procedure itemEndClick(Sender: TObject);
    procedure itemLoadClick(Sender: TObject);
    procedure itemSaveClick(Sender: TObject);
    procedure lbxSRepoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure lbxSselectedDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxSselectedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbxSselectedKeyPress(Sender: TObject; var Key: char);
    procedure loadRepo(repoPath: string);
    procedure menuFileClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TRepoFile = record
    Name: string;
    filePath: string;
  end;

var
  frmSongs: TfrmSongs;
  repo: array of TRepoFile;

implementation

{$R *.lfm}

{ TfrmSongs }


procedure TfrmSongs.loadRepo(repoPath: string);
var Info: TSearchRec;
    i,c: integer;
    songName: string;
begin
  if FindFirst(repoPath + PathDelim + '*', faAnyFile, Info)=0 then
    begin
    lbxSRepo.Clear;
    Repeat
      if (Info.Name[1] <> '.') then
        begin
         //Finde den letzten Punkt
         songName := Info.Name + '.';
         for i := 1 to length(Info.Name) do
           if songName[i] = '.' then c := i;
         songName := copy(songName,1,c-1);
         lbxSRepo.Items.Add(songName);
         setlength(repo, length(repo)+1);
         //Füllen des Repo-Arrays zur späteren Fehlerkorrektur!
         repo[(length(repo)-1)].Name := songName;
         repo[(length(repo)-1)].filePath := Info.Name;
        end;
    Until FindNext(info)<>0;
    end;
  FindClose(Info);
end;

procedure TfrmSongs.menuFileClick(Sender: TObject);
begin

end;

procedure TfrmSongs.MenuItem1Click(Sender: TObject);
begin
  frmInfo.ShowModal;
end;

procedure TfrmSongs.FormResize(Sender: TObject);
begin
  grbControl.Width:=40;
  lbxSRepo.Width:=(frmSongs.Width-grbControl.Width) div 2;
  lbxSSelected.left:=grbControl.Width+lbxSRepo.Width;
  lbxSSelected.Width:=lbxSRepo.Width;
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
      ShowMessage('Sie nutzen dieses Programm zum ersten Mal. Bitte wählen Sie einen Ordner aus, in dem sich die Liedtexte befinden.');
      frmSettings.ShowModal;
    end;
  loadRepo(frmSettings.edtRepoPath.Text);
end;

procedure TfrmSongs.grbControlClick(Sender: TObject);
begin

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
    ShowMessage('Fehler beim Öffnen. Wahrscheinlich haben Sie nicht die nötigen Rechte, um auf diese Datei zuzugreifen');
  end;
end;

procedure TfrmSongs.itemSaveClick(Sender: TObject);
begin
  try
    if SaveDialog.Execute then lbxSselected.Items.SaveToFile(SaveDialog.FileName);
  except
    ShowMessage('Fehler beim Speichern. Wahrscheinlich haben Sie nicht die nötigen Rechte, um auf diesen Ordner zuzugreifen');
  end;
end;

procedure TfrmSongs.lbxSRepoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Space) or (Key = VK_Return) then btnAddClick(lbxSRepo);
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

procedure TfrmSongs.FormCreate(Sender: TObject);
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
var i,j: integer;
    songfile: TStringList;
    stanza: string;
begin
  if lbxSSelected.Count > 0 then
  begin
  present.cur:=0;
  present.textList.Clear;
  songfile := TStringList.Create();
  for i := 0 to lbxSSelected.Count-1 do
    begin
    //suche Dateinamen in repo-Array
    j := 0;
    try
      while repo[j].Name <> lbxSSelected.Items.Strings[i] do
        inc(j);
    except
      ShowMessage('Fehler: Das Lied "' + lbxSSelected.Items.Strings[i] + '" ist nicht vorhanden. Es wird übersprungen.')
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
          stanza := '';
        end
        else stanza := stanza + songfile.Strings[j] + LineEnding;
    end;
    present.textList.Add(stanza);
    if frmSettings.cbEmptyFrame.Checked then
      present.textList.Add('');
    end;
  frmPresent.Show();
  songfile.Free;
  end else ShowMessage('Es müssen zuerst Lieder hinzugefügt werden.');
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

procedure TfrmSongs.FileNameEdit1Change(Sender: TObject);
begin

end;

end.

