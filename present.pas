unit Present;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Settings, Types, Themes, LCLTranslator, LCLIntf, ExtCtrls, Lyrics,
  IntfGraphics, AsyncProcess,
  fpImage,
  math;
type

  { TfrmPresent }

  TfrmPresent = class(TForm)
    ReloadImage: TAsyncProcess;
    imgBackground: TImage;
    lblNext: TLabel;
    lblMeta: TLabel;
    lblText: TLabel;
    ManipulatedBitmap: TBitmap;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblTextClick(Sender: TObject);
    procedure lblTextContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure lblTextDblClick(Sender: TObject);
    procedure ReloadImageReadData(Sender: TObject);
    procedure showItem(index: integer);
    procedure SwitchFullScreen;
    procedure SwitchFullScreen(WantFullScreen: Boolean);
    procedure LoadSettings;
    procedure ShowMeta;
  private
    { private declarations }
    function getCurrentSong: lyrics.TSong;
    procedure Brightness(SourceBitmap, DestBitmap: TBitMap; Offset: integer);
    procedure BrightnessBitmap(SourceBitmap, DestBitmap: TBitmap; Prozent:integer);
  public
    { public declarations }
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    ScreenBounds: TRect;
    Songlist: lyrics.TSongList;
    procedure LoadBackground;
    procedure GoPrevious;
    procedure GoNext;
    procedure Refresh;
    procedure ShowFirst;
  end;

var
  frmPresent: TfrmPresent;
  textList: TStringList;
  songMetaList: TStringList;
  cur: Integer; //The current Index of the String List which is shown
  FullScreen: Boolean;

implementation

Uses
  SongSelection;
{$R *.lfm}

{ TfrmPresent }

procedure TfrmPresent.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((key = VK_RIGHT) or (key = VK_DOWN) or (key = VK_SPACE) or (key = VK_RETURN)) then
    GoNext
  else if ((key = VK_LEFT) or (key = VK_UP)) then GoPrevious
  else if ((key = VK_F11) or (key = VK_F5)) then SwitchFullscreen()
  else if (key = VK_Escape) then frmPresent.Hide;
end;

procedure TfrmPresent.GoNext;
begin
  if (cur < textList.Count-1) then
    begin
      inc(cur);
      ShowItem(cur);
    end;
  if SongSelection.ProgrammMode = ModeMultiscreenPresentation Then frmSongs.ImageUpdater.Enabled:=True;
end;

procedure TfrmPresent.GoPrevious;
begin
  if (cur > 0) then
  begin
    dec(cur);
    ShowItem(cur);
  end;
  if SongSelection.ProgrammMode = ModeMultiscreenPresentation Then frmSongs.ImageUpdater.Enabled:=True;
end;

procedure TfrmPresent.FormResize(Sender: TObject);
begin
  showItem(cur);
  LoadBackground;
end;

procedure TfrmPresent.FormShow(Sender: TObject);
begin
  LoadSettings;
  if textList.Count>0 then showItem(0) else frmPresent.Hide;
  Refresh;
end;

procedure TFrmPresent.LoadSettings;
begin
  frmPresent.Color:=frmSettings.bgColorDialog.Color;
  frmPresent.lblText.Font.Color:=frmSettings.textColorDialog.Color;
  lblText.Font := frmSettings.FontDialog.Font;
  lblText.Font.Color:= frmSettings.textColorDialog.Color;
  lblMeta.Font := frmSettings.FontDialog.Font;
  lblMeta.Font.Color := frmSettings.textColorDialog.Color;
  lblMeta.Font.Height:= lblMeta.Font.Height div 3;
  lblMeta.Width := Trunc(frmPresent.Width * 0.67);
  LoadBackground;
end;

procedure TfrmPresent.lblTextClick(Sender: TObject);
begin
  if (cur < textList.Count-1) then
    begin
      inc(cur);
      showItem(cur);
    end;
end;

procedure TfrmPresent.lblTextContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TfrmPresent.lblTextDblClick(Sender: TObject);
begin
  SwitchFullScreen;
end;

procedure TfrmPresent.ReloadImageReadData(Sender: TObject);
begin
  LoadBackground;
end;


procedure TfrmPresent.showItem(index: integer);
begin
    cur := index;
    lblText.WordWrap:=True;
    lblText.Font := frmSettings.FontDialog.Font;
    lblText.Font.Color:= frmSettings.textColorDialog.Color;
    lblMeta.Font.Color := frmSettings.textColorDialog.Color;
    lblMeta.Font.Size := lblText.Font.Size div 3;
    lblMeta.Width := Trunc(frmPresent.Width * 0.67);
    if ((frmSettings.cbSpoiler.Checked) and (textList.Count > cur + 1) and (textList.Strings[cur] <> '')) then
    begin
      lblNext.Visible:=True;
      //lblNext.Caption := copy(textList.Strings[cur+1], 1, pos(LineEnding, textList.Strings[cur+1])-1);
      if (SongMetaList.Strings[cur+1] <> SongMetaList.Strings[cur]) and (frmSettings.cbEmptyFrame.Checked=False) then
         lblNext.Caption := ''
      else lblNext.Caption := textList.Strings[cur+1];
      lblNext.Font := frmSettings.FontDialog.Font;
      lblNext.Font.Color := frmSettings.textColorDialog.Color;
      lblNext.Font.Height:= lblNext.Font.Height div 2;
      lblNext.BorderSpacing.Top:=2*lblNext.Font.Size;
    end else
    begin
      lblNext.Caption := '';
      lblNext.Visible:= False;
      lblText.Height := frmPresent.Height;
      lblNext.BorderSpacing.Top := 0;
    end;
    lblText.Caption := textList.Strings[cur];
    lblText.BorderSpacing.Top := (frmPresent.Height-lblText.Height-lblNext.Height-lblNext.BorderSpacing.Top) div 2;
    // Aktualisiere SongListe in Present-Form
    SongSelection.frmSongs.UpdateSongPositionInLbxSSelected;
    ShowMeta;
    lblMeta.Top := frmPresent.Height-lblMeta.Height-lblMeta.Left;
end;

procedure TfrmPresent.ShowMeta;
var showM: Boolean;
  MetaSyntax: String;
begin
  showM := False;
  {Check if meta should be shown at the beginning of song }
  if ((frmSettings.cbMetaDataFirstSlide.Checked) and ((cur <= 0) or (SongMetaList.Strings[cur-1] <> SongMetaList.Strings[cur])))
     then showM := True
  {Check if meta should be shown at the end of song }
  else if (frmSettings.cbMetaDataLastSlide.Checked) and (((frmSettings.cbEmptyFrame.Checked) and (cur < SongMetaList.Count-1) and (textList.Strings[cur+1] = '')) or ((frmSettings.cbEmptyFrame.Checked = False) and ((cur = SongMetaList.count-1) or (SongMetaList.Strings[cur+1] <> SongMetaList.Strings[cur]))))
     then showM := True;

  MetaSyntax := frmSettings.memoMetaData.Lines.Text;
  lblMeta.Caption := getCurrentSong.ParseMetaData(MetaSyntax);
  lblMeta.Visible := showM;
end;

procedure TfrmPresent.FormCreate(Sender: TObject);
begin
  cur := 0;
  present.textList := TStringList.Create;
  present.songMetaList := TStringList.Create;
  FullScreen := False;
  self.WindowState:= wsMaximized;
end;

procedure TfrmPresent.FormDestroy(Sender: TObject);
begin
  textList.Free;
end;

procedure TfrmPresent.FormHide(Sender: TObject);
begin
  // Stelle frmSongs wieder her
  SongSelection.ProgrammMode:=SongSelection.ModeSelection;
  SongSelection.frmSongs.FormResize(self);
  SongSelection.frmSongs.KeyPreview := False;

  // Aktiviere Präsentations-Button, um Präsentation erneut starten zu können

  SongSelection.frmSongs.itemPresentation.Enabled := True;
  SongSelection.frmSongs.btnStartPresentation.Enabled := True;

  // Deaktiviere Vollbildschirm (falls noch möglich)

  SwitchFullScreen(False);
  SongSelection.frmSongs.UpdateControls;
  if Assigned(Songlist) then Songlist.Free;
end;

procedure TfrmPresent.SwitchFullScreen;
begin
  {$if defined(WINDOWS)}
  if BorderStyle <> bsNone then begin
    // To full screen
    OriginalWindowState := WindowState;
    OriginalBounds := BoundsRect;
    BorderStyle := bsNone;
    BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
    Fullscreen := True;
  end else begin
    // From full screen
    BorderStyle := bsSizeable;
    if OriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      BoundsRect := OriginalBounds;
    Fullscreen := False;
  end;
  {$endif}
  {$if defined(LINUX)}
  if Fullscreen = False then begin
    // To full screen
    ShowWindow(Handle, SW_SHOWFULLSCREEN);
    Fullscreen := True;
  end else begin
    // From full screen
    ShowWindow(Handle, SW_SHOWMAXIMIZED);
    Fullscreen := False;
  end;
  {$endif}
end;

procedure TfrmPresent.SwitchFullScreen(WantFullScreen: Boolean);
begin
  {$if defined(WINDOWS)}
  if WantFullScreen = True then begin
    // To full screen
    OriginalWindowState := WindowState;
    OriginalBounds := BoundsRect;
    BorderStyle := bsNone;
    BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
    Fullscreen := True;
  end else begin
    // From full screen
    BorderStyle := bsSizeable;
    if OriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      BoundsRect := OriginalBounds;
    Fullscreen := False;
  end;
  {$endif}
  {$if defined(LINUX)}
  if WantFullScreen = True then begin
    // To full screen
    ShowWindow(Handle, SW_SHOWFULLSCREEN);
    Fullscreen := True;
  end else begin
    // From full screen
    ShowWindow(Handle, SW_SHOWMAXIMIZED);
    Fullscreen := False;
  end;
  {$endif}
end;

procedure TfrmPresent.Refresh;
begin
  ShowItem(cur);
end;

procedure TfrmPresent.ShowFirst;
begin
  cur := 0;
  Refresh;
end;

function TfrmPresent.getCurrentSong: lyrics.TSong;
var i, count: integer;
begin
  count := 0;
  if cur <= 0 then exit(SongList.Items[count]);
  for i := 1 to cur do
  begin
      if SongMetaList.Strings[i] <> SongMetaList.Strings[i-1] then count := count+1;
  end;
  Result := SongList.Items[count];
end;

procedure TfrmPresent.LoadBackground;
//var originalPicture: TPicture;
begin
  // Handle Background Image
    if (frmSettings.cbShowBackgroundImage.Checked) and (FileExists(frmSettings.BgPictureDialog.FileName)) then
    begin
      imgBackground.Visible:=True;
      imgBackground.Width:=frmPresent.Width;
      imgBackground.Height:=frmPresent.Height;
      //originalPicture := TPicture.Create;
      //originalPicture.LoadFromFile(frmSettings.BgPictureDialog.FileName);
      imgBackground.Picture.LoadFromFile(frmSettings.BgPictureDialog.FileName);
      if imgBackground.Width >= imgBackground.Picture.Width then
        imgBackground.Height:=Trunc(imgBackground.width*imgBackground.Picture.Height/imgBackground.Picture.Width)
      else if imgBackground.Height > imgBackground.Picture.Height then
        imgBackground.Width:=Trunc(frmPresent.Height*imgBackground.Picture.Width/imgBackground.Picture.Height);
      BrightnessBitmap(imgbackground.Picture.Bitmap, imgbackground.Picture.Bitmap, frmSettings.sbImageBrightness.Position);

      //originalPicture.Free;
    end
    else
    begin
      imgBackground.Visible:=False;
    end;
end;

procedure TfrmPresent.Brightness(SourceBitmap, DestBitmap: TBitMap; Offset: integer);
var
  SrctfImg, TemptfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  px, py: integer;
  CurColor: TFPColor;
begin
  Offset:=Offset * $FF;
  SrctfImg := TLazIntfImage.Create(0, 0);
  SrctfImg.LoadFromBitmap(SourceBitmap.Handle, SourceBitmap.MaskHandle);
  TemptfImg := TLazIntfImage.Create(0, 0);
  TemptfImg.DataDescription := GetDescriptionFromDevice(0);
  TemptfImg.SetSize(SrctfImg.Width,SrctfImg.Height);
  for py := 0 to SrctfImg.Height - 1 do
  begin
    for px := 0 to SrctfImg.Width - 1 do
    begin
      CurColor := SrctfImg.Colors[px, py];
      CurColor.red := EnsureRange(CurColor.red + Offset,0,$FFFF);
      CurColor.green := EnsureRange(CurColor.green + Offset,0,$FFFF);
      CurColor.blue := EnsureRange(CurColor.blue + Offset,0,$FFFF);
      TemptfImg.Colors[px, py] := CurColor;
    end;
  end;
  TemptfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  DestBitmap.Handle := ImgHandle;
  DestBitmap.MaskHandle := ImgMaskHandle;
  SrctfImg.Free;
  TemptfImg.Free;
end;

Procedure TfrmPresent.BrightnessBitmap(SourceBitmap, DestBitmap: TBitmap; Prozent:integer);
var
  Stream:Tmemorystream;
  neuwert:array[0..255] of byte;
  i:Integer;
  b:byte;
begin
//  i:=gettickcount;

  if Prozent<0 then        //Abdunkeln
    for i:=0 to 255 do neuwert[i]:=round(i * (100+prozent)/100)
  else                     //Aufhellen
    for i:=0 to 255 do neuwert[i]:=round(i + (255-i) * (prozent)/100);

  Stream:=TMemorystream.Create;
  try
    Sourcebitmap.SaveToStream(Stream);

    Stream.Position:=55;   //Header Bitmap weg, nur Daten nutzen (http://de.wikipedia.org/wiki/Windows_Bi ... tionsblock)

    for i:=Stream.Position to Stream.Size-1 do begin
      b:=Stream.Readbyte;
      Stream.Position:=i;
      Stream.WriteByte(neuwert[b]);
    end;

    Stream.Position:=0;
    DestBitmap.LoadFromStream(Stream);

  finally
    Stream.Free;
  end;
end;

end.
