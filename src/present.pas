unit Present;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Settings, Types, Themes, LCLTranslator, LCLIntf, ExtCtrls, Lyrics,
  IntfGraphics,
  fpImage, StrUtils, Slides,
  math;
type

  { TfrmPresent }

  TfrmPresent = class(TForm)
    imgBackground: TImage;
    lblNext: TLabel;
    lblMeta: TLabel;
    lblText: TLabel;
    ManipulatedBitmap: TBitmap;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelHorz(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblTextClick(Sender: TObject);
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
    SlideList: TSlideList;
    procedure LoadBackground;
    procedure ResizeBackground;
    procedure GoPrevious;
    procedure GoNext;
    procedure Refresh;
    procedure ShowFirst;
  end;

var
  frmPresent: TfrmPresent;
  cur: Integer; //The current Index of the String List which is shown
  FullScreen: Boolean;

ResourceString
  StrMoreLyricsIndicator = '...';

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

procedure TfrmPresent.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TfrmPresent.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TfrmPresent.FormMouseWheelHorz(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  {if WheelDelta > 0 then GoNext
  else if WheelDelta < 0 then GoPrevious;
  Handled := True;}
end;

procedure TfrmPresent.FormPaint(Sender: TObject);
begin

end;

procedure TfrmPresent.GoNext;
begin
  if (cur < frmPresent.SlideList.Count-1) then
    begin
      inc(cur);
      ShowItem(cur);
    end;
  if SongSelection.ProgramMode = ModeMultiscreenPresentation Then frmSongs.ReloadPresentationImage;
end;

procedure TfrmPresent.GoPrevious;
begin
  if (cur > 0) then
  begin
    dec(cur);
    ShowItem(cur);
  end;
  if SongSelection.ProgramMode = ModeMultiscreenPresentation Then frmSongs.ReloadPresentationImage;
end;

procedure TfrmPresent.FormResize(Sender: TObject);
begin
  showItem(cur);
  ResizeBackground;
end;

procedure TfrmPresent.FormShow(Sender: TObject);
begin
  if SlideList.Count >0 then showItem(0) else frmPresent.Hide;
  ResizeBackground;
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
  if (cur < SlideList.Count-1) then
    begin
      inc(cur);
      showItem(cur);
    end;
end;

procedure TfrmPresent.lblTextDblClick(Sender: TObject);
begin
  SwitchFullScreen;
end;

procedure TfrmPresent.ReloadImageReadData(Sender: TObject);
begin
  ResizeBackground;
end;


procedure TfrmPresent.showItem(index: integer);
var StringArray: TStringDynArray;
begin
    cur := index;
    lblText.WordWrap:=True;
    lblText.Font := frmSettings.FontDialog.Font;
    lblText.Font.Color:= frmSettings.textColorDialog.Color;
    lblMeta.Font.Color := frmSettings.textColorDialog.Color;
    lblMeta.Font.Size := lblText.Font.Size div 3;
    lblMeta.Width := Trunc(frmPresent.Width * 0.67);
    lblText.Caption := SlideList.Items[cur].PartContent.MainText;
    lblNext.Caption := SlideList.Items[cur].PartContent.SpoilerText;
    lblMeta.Caption:= SlideList.Items[cur].PartContent.MetaText;
    if lblNext.Caption <> '' then // if there is a spoiler/next text to display
    begin
      lblNext.Visible:=True;
      //lblNext.Caption := copy(textList.Strings[cur+1], 1, pos(LineEnding, textList.Strings[cur+1])-1);
      lblNext.Font := frmSettings.FontDialog.Font;
      lblNext.Font.Color := frmSettings.textColorDialog.Color;
      lblNext.Font.Height:= lblNext.Font.Height div 2;
      lblNext.Top := lblText.Top+lblText.Height;
      lblNext.BorderSpacing.Top:=2*lblNext.Font.Size;
      if lblNext.Top+lblNext.Height > frmPresent.Height then // the spoiler is going beyond the form
      begin
        lblNext.BorderSpacing.Top:=0;
        StringArray := SplitString(lblNext.Caption,LineEnding);
        if length(StringArray) > 0 then
           lblNext.Caption := StringArray[0] + StrMoreLyricsIndicator;
        // frmPresent.Repaint;
        // Application.ProcessMessages;
        // if still to much content then hide
        if lblNext.Top+lblNext.Height > frmPresent.Height then
          begin
            lblNext.Visible:=False;
            lblText.Height := frmPresent.Height;
            lblNext.BorderSpacing.Top := 0;
          end;
      end;
    end else // There is no spoiler/next text to display
    begin
      lblNext.Visible:= False;
      lblText.Height := frmPresent.Height;
      lblNext.BorderSpacing.Top := 0;
    end;

    lblText.BorderSpacing.Top := (frmPresent.Height-lblText.Height-lblNext.Height-lblNext.BorderSpacing.Top) div 2;
    // Aktualisiere SongListe in Present-Form
    SongSelection.frmSongs.UpdateSongPositionInLbxSSelected;
    ShowMeta;
    lblMeta.Top := frmPresent.Height-lblMeta.Height-lblMeta.Left;
    frmSongs.ImageUpdater.Enabled:=True;
end;

procedure TfrmPresent.ShowMeta;
var showM: Boolean;
begin
  lblMeta.Caption := SlideList.Items[cur].PartContent.MetaText;
  lblMeta.Visible := (SlideList.Items[cur].PartContent.MetaText <> '');
end;

procedure TfrmPresent.FormCreate(Sender: TObject);
begin
  cur := 0;
  FullScreen := False;
  self.WindowState:= wsMaximized;
  //LoadSettings;
end;

procedure TfrmPresent.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TfrmPresent.FormDestroy(Sender: TObject);
begin
end;

procedure TfrmPresent.FormHide(Sender: TObject);
begin
  // Stelle frmSongs wieder her
  SongSelection.ProgramMode:=SongSelection.ModeSelection;
  SongSelection.frmSongs.FormResize(self);
  SongSelection.frmSongs.KeyPreview := False;

  // Aktiviere Präsentations-Button, um Präsentation erneut starten zu können

  SongSelection.frmSongs.itemPresentation.Enabled := True;
  SongSelection.frmSongs.btnStartPresentation.Enabled := True;

  // Deaktiviere Vollbildschirm (falls noch möglich)

  SwitchFullScreen(False);
  SongSelection.frmSongs.UpdateControls;
  if Assigned(SlideList) then SlideList.Free;
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
  Result := SlideList.Items[cur].Song;
end;

procedure TfrmPresent.LoadBackground;
begin
  // Handle Background Image
    if (frmSettings.cbShowBackgroundImage.Checked) and (FileExists(frmSettings.BgPictureDialog.FileName)) then
    begin
      imgBackground.Visible:=True;
      try
      imgBackground.Picture.LoadFromFile(frmSettings.BgPictureDialog.FileName);
      except on error: FPImageException do
      begin
        imgBackground.Visible := False;
        frmSettings.cbShowBackgroundImage.Checked := False;
        exit;
      end;
      end;
      //ResizeBackground;
      BrightnessBitmap(imgbackground.Picture.Bitmap, imgbackground.Picture.Bitmap, frmSettings.sbImageBrightness.Position);
    end
    else
    begin
      imgBackground.Visible:=False;
    end;
    // set changedBackground as False unless changes are done again (in settings)
    frmSettings.changedBackground:=False;
end;

procedure TfrmPresent.ResizeBackground;
var newHeight, newWidth: integer;
begin
  imgBackground.Width:=frmPresent.Width;
  imgBackground.Height:=frmPresent.Height;
  if (imgBackground.Height = 0) or (imgBackground.Picture.Height = 0) then Exit; // Prevent Errors
  if imgBackground.Width/imgBackground.Height >= imgBackground.Picture.Width/imgBackground.Picture.Height then
        begin
          newHeight:=Trunc(imgBackground.Width*imgBackground.Picture.Height/imgBackground.Picture.Width);
          imgBackground.Top:=-Abs(Trunc((imgBackground.Height-frmPresent.Height)/2));
          imgBackground.Left:=0;
          imgBackground.Height := newHeight;
        end
      else
        begin
          if frmSettings.cbShowBackgroundImage.Checked then // This is important because else there will be a range check error!
          begin
            newWidth:=Trunc(frmPresent.Height*imgBackground.Picture.Width/imgBackground.Picture.Height);
            imgBackground.Left:=-Abs(Trunc((imgBackground.Width-frmPresent.Width)/2));
            imgBackground.Top:=0;
            imgBackground.Width:=newWidth;
          end;
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

    Stream.Position:=55;

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
