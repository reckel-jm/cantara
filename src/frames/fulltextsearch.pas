unit fulltextsearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Lyrics, FGL,
  Types, LCLType, Graphics, Math;

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
    EnterKeyword: TPage;
    LabalCreateIndex: TLabel;
    LabelEnterKeyword: TLabel;
    LabelEnterSearchTerm: TLabel;
    LabelNoItemsFound: TLabel;
    ListBoxResults: TListBox;
    LoadIndex: TPage;
    NoResult: TPage;
    SearchResults: TPage;
    procedure EditSearchTermChange(Sender: TObject);
    procedure ListBoxResultsDblClick(Sender: TObject);
    procedure ListBoxResultsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private

  public
    IndexList: TIndexList;
    procedure CreateIndex;
    constructor Create;
    destructor Destroy; override;
  end;

resourcestring
  StrSongTitle = 'Song Title';

implementation

{$R *.lfm}

uses
  SongSelection;

destructor TIndexEntry.Destroy;
begin
  self.Song.FreeInstance;
  inherited;
end;

procedure TFrmFulltextsearch.CreateIndex;
var
  TempSong: TSong;
  RepoEntry: TRepoFile;
  IndexEntry: TIndexEntry;
begin
  if Assigned(IndexList) then FreeAndNil(IndexList);
  IndexList := TIndexList.Create(True);
  // Iterate over all loaded songs
  for RepoEntry In Songselection.Repo do
  begin
    IndexEntry := TIndexEntry.Create;
    TempSong := TSong.Create;
    TempSong.importSongfile(RepoEntry.FilePath);
    IndexEntry.Song := TempSong;
    TempSong.output.Delimiter := Char(' ');
    IndexEntry.ContentIndex := Trim(TempSong.output.Text);
    IndexEntry.ContentIndex :=
      StringReplace(IndexEntry.ContentIndex, LineEnding, ' ', [rfReplaceAll]);
    IndexList.Add(IndexEntry);
  end;
  // Change to the Notebook page with the Listbox
  Content.PageIndex := 3;
end;

constructor TFrmFulltextsearch.Create;
begin
  inherited;
  {$ifdef WINDOWS}
  self.ListBoxResults.Options:=[lboDrawFocusRect];
  {$endif}
end;

destructor TFrmFulltextsearch.Destroy;
begin
  if Assigned(IndexList) then FreeAndNil(IndexList);
  inherited Destroy;
end;

procedure TFrmFulltextsearch.EditSearchTermChange(Sender: TObject);
var
  i: Integer;
  SearchTerm, ContentString, AddedContent: String;
  PosInFileName, PosInContentIndex: Integer;
begin
  SearchTerm := Trim(EditSearchTerm.Text);
  if SearchTerm = '' then
  begin
    Content.PageIndex := 3;
    Exit;
  end;
  Content.PageIndex := 1;
  ListboxResults.Clear;
  for i := 0 to IndexList.Count - 1 do
  begin
    ContentString := IndexList.Items[i].ContentIndex;
    PosInFileName := Pos(LowerCase(SearchTerm),
      LowerCase(IndexList.Items[i].Song.FileNameWithoutEnding));
    PosInContentIndex := Pos(LowerCase(SearchTerm), LowerCase(ContentString));
    if (PosInFileName > 0) Or (PosInContentIndex > 0) then
    begin
      AddedContent := IndexList.Items[i].Song.FileNameWithoutEnding;
      AddedContent += PathSeparator;
      ContentString := IndexList.Items[i].ContentIndex;
      if PosInContentIndex > 0 then
        AddedContent += ContentString[PosInContentIndex -
          (Min(PosInContentIndex, 70)) + 1..PosInContentIndex + Min(
          Length(ContentString) - PosInContentIndex, 70) - 1]
      else if PosInFileName > 0 then
        AddedContent += StrSongTitle;
      ListBoxResults.Items.Add(AddedContent);
    end;
  end;
  if ListBoxResults.Count = 0 then Content.PageIndex := 2;
end;

procedure TFrmFulltextsearch.ListBoxResultsDblClick(Sender: TObject);
var
  { The song which has been selected in the search }
  selectedSongName: String;
begin
  selectedSongName := ListBoxResults.Items[ListBoxResults.ItemIndex].Split(
      PathSeparator)[0];

  if frmSongs.ProgramMode = TProgramMode.ModeSelection then
    frmSongs.lbxSselected.Items.Add(ListBoxResults.Items[ListBoxResults.ItemIndex].Split(
      PathSeparator)[0])
  else if frmSongs.ProgramMode = TProgramMode.ModeMultiscreenPresentation then
  begin
    if frmSongs.lbxSSelected.Items.IndexOf(selectedSongName) > -1 then
    begin
      frmSongs.lbxSselected.ItemIndex:=frmSongs.lbxSSelected.Items.IndexOf(selectedSongName);
      frmSongs.lbxSselected.OnClick(self);
    end;
  end;
end;

procedure TFrmFulltextsearch.ListBoxResultsDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  aColor: TColor;
  FontBaseHeight: Integer;
  StringParts: TStringArray;
begin
  ARect.Height := ListBoxResults.ItemHeight;
  if odSelected In State then
  begin
    aColor := clActiveCaption;
    ListBoxResults.Canvas.Font.Color := clCaptionText;
  end
  else
  begin
    aColor := clDefault;
    ListBoxResults.Canvas.Font.Color := clWindowText;
  end;
  FontBaseHeight := abs(GetFontData(Font.Reference.Handle).Height);
  ListBoxResults.Canvas.Brush.Color := aColor;
  ListBoxResults.Canvas.FillRect(aRect);
  ListBoxResults.Canvas.Brush.Color := ListBoxResults.Canvas.Font.Color;
  ListBoxResults.Canvas.Font.Bold := True;
  ListBoxResults.Canvas.Font.Underline := True;
  with ListBoxResults.Canvas.TextStyle do
  begin
    Alignment := taCenter;
    Layout := tlTop;
    SingleLine := True;
    WordBreak := False;
    Opaque := False;
  end;
  // Now we split the strings into the two parts
  StringParts := ListBoxResults.Items[Index].Split(PathSeparator);
  ListBoxResults.Canvas.TextRect(ARect, 2, ARect.Top + 2, StringParts[0]);
  if length(StringParts) > 1 then
  begin
    ListBoxResults.Canvas.Font.Bold := False;
    ListBoxResults.Canvas.Font.Underline := False;
    ListBoxResults.Canvas.TextRect(ARect, 2, ARect.Top + 2 +
      Round(FontBaseHeight * 1.33),
      StringParts[1]);
  end;
end;

end.
