unit fulltextsearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Lyrics, FGL, Types, LCLType, Graphics, Math;

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
    procedure EditSearchTermChange(Sender: TObject);
    procedure ListBoxResultsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure NoResultBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
  private

  public
    IndexList: TIndexList;
    procedure CreateIndex;
    destructor Destroy; override;
  end;

ResourceString
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
    TempSong.output.Delimiter := Char(LineEnding);
    IndexEntry.ContentIndex:=Trim(TempSong.output.Text);
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
  ContentString, AddedContent: String;
  PosInFileName, PosInContentIndex: Integer;
begin
  Content.PageIndex:=1;
  ListboxResults.Clear;
  for i := 0 to IndexList.Count-1 do
  begin
    ContentString := IndexList.Items[i].ContentIndex;
    PosInFileName := Pos(LowerCase(EditSearchTerm.Text), LowerCase(IndexList.Items[i].Song.FileNameWithoutEnding));
    PosInContentIndex := Pos(LowerCase(EditSearchTerm.Text), LowerCase(ContentString));
    if (PosInFileName > 0) or
      (PosInContentIndex > 0) then
       begin
         AddedContent := IndexList.Items[i].Song.FileNameWithoutEnding;
         AddedContent += LineEnding;
         ContentString := IndexList.Items[i].ContentIndex;
         if PosInContentIndex > 0 then
           AddedContent += ContentString[PosInContentIndex-(Min(PosInContentIndex,60))+1..PosInContentIndex+Min(Length(ContentString)-PosInContentIndex, 60)-1]
         else if PosInFileName > 0 then
           AddedContent += StrSongTitle;
         ListBoxResults.Items.Add(AddedContent);
       end;
  end;
  if ListBoxResults.Count = 0 then Content.PageIndex := 2;
end;

procedure TFrmFulltextsearch.ListBoxResultsDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  aColor: TColor;
  headingStyle: TTextStyle;
  FontBaseHeight: Integer;
  StringParts: TStringArray;
begin
  if odSelected in State then aColor := clActiveCaption
  else aColor := clDefault;
  FontBaseHeight := abs(GetFontData(Font.Reference.Handle).Height);
  ListBoxResults.Canvas.Brush.Color := aColor;
  ListBoxResults.Canvas.FillRect(aRect);
  ListBoxResults.Canvas.Font.Bold:=True;
  HeadingStyle.Alignment:=taCenter;
  // Now we split the strings into the two parts
  StringParts := ListBoxResults.Items[Index].Split(LineEnding);
  ListBoxResults.Canvas.TextRect(ARect, 2, ARect.Top+2, StringParts[0], HeadingStyle);
  if length(StringParts) > 1 then
  begin
    ListBoxResults.Canvas.Font.Bold:=False;
    ListBoxResults.Canvas.TextRect(ARect, 2, ARect.Top+2+Round(FontBaseHeight*1.33), ListBoxResults.Items[Index]);
  end;
end;

procedure TFrmFulltextsearch.NoResultBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin

end;

end.

