unit fulltextsearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Lyrics, FGL;

type

  TIndexEntry = class
    public
      Song: TSong;
      ContentIndex: String;
      TagIndex: String;
      destructor Destroy; override;
  end;

  TIndexList = specialize TFPGObjectList<TIndexEntry>;

  { TFrmFulltextsearch }

  TFrmFulltextsearch = class(TFrame)
    EditSearchTerm: TEdit;
    Content: TNotebook;
    LabalCreateIndex: TLabel;
    LabelEnterSearchTerm: TLabel;
    LabelNoItemsFound: TLabel;
    ListBoxResults: TListBox;
    LoadIndex: TPage;
    NoResult: TPage;
    SearchResults: TPage;
    destructor Destroy; override;
    procedure EditSearchTermChange(Sender: TObject);
    procedure NoResultBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
  private

  public
    IndexList: TIndexList;
    procedure CreateIndex;
  end;

implementation

{$R *.lfm}

uses
  SongSelection;

destructor TIndexEntry.Destroy;
begin
  self.Song.FreeInstance;
  inherited;
end;

procedure TFrmFulltextSearch.CreateIndex;
var TempSong: TSong;
  RepoEntry: TRepoFile;
  IndexEntry: TIndexEntry;
begin
  if Assigned(IndexList) then FreeAndNil(IndexList);
  IndexList := TIndexList.Create(True);
  // Iterate over all loaded songs
  for RepoEntry in Songselection.Repo do
  begin
    IndexEntry := TIndexEntry.Create;
    TempSong := TSong.Create;
    TempSong.importSongfile(RepoEntry.FilePath);
    IndexEntry.Song := TempSong;
    TempSong.output.Delimiter := LineEnding;
    IndexEntry.ContentIndex:=Trim(LowerCase(TempSong.output.Text));
    IndexEntry.ContentIndex:=StringReplace(IndexEntry.ContentIndex, LineEnding, ' ', [rfReplaceAll, rfIgnoreCase]);
    IndexList.Add(IndexEntry);
  end;
  // Change to the Notebook page with the Listbox
  Content.PageIndex:=1;
end;

destructor TFrmFulltextSearch.Destroy;
begin
  if Assigned(IndexList) then FreeAndNil(IndexList);
  inherited Destroy;
end;

procedure TFrmFulltextsearch.EditSearchTermChange(Sender: TObject);
var i: Integer;
begin
  Content.PageIndex:=1;
  ListboxResults.Clear;
  for i := 0 to IndexList.Count-1 do
  begin
    if (Pos(LowerCase(EditSearchTerm.Text), LowerCase(IndexList.Items[i].Song.FileNameWithoutEnding)) > 0) or
      (Pos(LowerCase(EditSearchTerm.Text), LowerCase(IndexList.Items[i].ContentIndex)) > 0) then
       ListBoxResults.Items.Add(IndexList.Items[i].Song.FileNameWithoutEnding);
  end;
  if ListBoxResults.Count = 0 then Content.PageIndex := 2;
end;

procedure TFrmFulltextsearch.NoResultBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin

end;

end.

