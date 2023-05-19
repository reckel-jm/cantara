unit Present;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Types, Themes, LCLTranslator, LCLIntf, ExtCtrls, Lyrics,
  IntfGraphics,
  fpImage, StrUtils, Slides,
  math;
type

  THackWinControl = class(TWinControl);

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
    cur: Integer; //The current Index of the String List which is shown
    FullScreen: Boolean;
    procedure LoadBackground;
    procedure ResizeBackground;
    procedure GoPrevious;
    procedure GoNext;
    procedure Refresh;
    procedure ShowFirst;
  end;

{ We need to overload in here with Word, so it can be used to determine if a pressed key is in one
of the constant arrays defined below. }
operator in (const AWord: Word; const AArray: array of Word): Boolean; inline;

{ This procedure is used to make a picture of the Form while it is closed. }
function ImageOfWinControl(aWinControl: TWinControl): TBitmap;

const
  { Here we define the list of keys which can be used to move to the next slide (GoRightKeys), go to the previous slide (GoLeftKeys,
  toggle fullscreen (ToggleFullscreenKeys) or quit the presentation (EscapeKeys). }
  GoRightKeys: array[0..6] of Word = (VK_RIGHT, VK_DOWN, VK_SPACE, VK_RETURN, VK_MEDIA_NEXT_TRACK, VK_BROWSER_FORWARD, VK_NEXT);
  GoLeftKeys: array[0..4] of Word = (VK_LEFT, VK_UP, VK_MEDIA_PREV_TRACK, VK_BROWSER_BACK, VK_PRIOR);
  ToggleFullscreenKeys: array[0..1] of Word = (VK_F11, VK_F5);
  EscapeKeys: array of Word = (VK_Escape);

var
  frmPresent: TfrmPresent;

ResourceString
  StrMoreLyricsIndicator = '...';

implementation

Uses
  SongSelection, Settings;
{$R *.lfm}

operator in (const AWord: Word; const AArray: array of Word): Boolean; inline;
var
  Item: Word;
begin
  for Item in AArray do
    if Item = AWord then
      Exit(True);
  Result := False;
end;

{ TfrmPresent }

procedure TfrmPresent.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key in GoRightKeys then
    GoNext
  else if key in GoLeftKeys then GoPrevious
  else if key in ToggleFullscreenKeys then SwitchFullscreen()
  else if key in EscapeKeys then self.Hide;
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
  if (cur < self.SlideList.Count-1) then
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
  if SlideList.Count >0 then showItem(0) else self.Hide;
  ResizeBackground;
  Refresh;
end;

procedure TFrmPresent.LoadSettings;
begin
  self.Color:=frmSettings.bgColorDialog.Color;
  self.lblText.Font.Color:=frmSettings.textColorDialog.Color;
  lblText.Font := frmSettings.FontDialog.Font;
  lblText.Font.Color:= frmSettings.textColorDialog.Color;
  lblMeta.Font := frmSettings.FontDialog.Font;
  lblMeta.Font.Color := frmSettings.textColorDialog.Color;
  lblMeta.Font.Height:= lblMeta.Font.Height div 3;
  lblMeta.Width := Trunc(self.Width * 0.67);
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
    lblMeta.Width := Trunc(self.Width * 0.67);
    try
    lblText.Caption := SlideList.Items[cur].PartContent.MainText;
    lblNext.Caption := SlideList.Items[cur].PartContent.SpoilerText;
    lblMeta.Caption:= SlideList.Items[cur].PartContent.MetaText;
    except
      ShowMessage(IntToStr(cur));
    end;
    if lblNext.Caption <> '' then // if there is a spoiler/next text to display
    begin
      lblNext.Visible:=True;
      lblNext.Font := frmSettings.FontDialog.Font;
      lblNext.Font.Color := frmSettings.textColorDialog.Color;
      lblNext.Font.Height:= lblNext.Font.Height div 2;
      lblNext.Top := lblText.Top+lblText.Height;
      lblNext.BorderSpacing.Top:=2*lblNext.Font.Size;
      if lblNext.Top+lblNext.Height > self.Height then // the spoiler is going beyond the form
      begin
        lblNext.BorderSpacing.Top:=0;
        StringArray := SplitString(lblNext.Caption,LineEnding);
        if length(StringArray) > 0 then
           lblNext.Caption := StringArray[0] + StrMoreLyricsIndicator;
        // self.Repaint;
        // Application.ProcessMessages;
        // if still to much content then hide
        if lblNext.Top+lblNext.Height > self.Height then
          begin
            lblNext.Visible:=False;
            lblText.Height := self.Height;
            lblNext.BorderSpacing.Top := 0;
          end;
      end;
    end else // There is no spoiler/next text to display
    begin
      lblNext.Visible:= False;
      lblText.Height := self.Height;
      lblNext.BorderSpacing.Top := 0;
    end;

    lblText.BorderSpacing.Top := (self.Height-lblText.Height-lblNext.Height-lblNext.BorderSpacing.Top) div 2;
    // Aktualisiere SongListe in Present-Form
    if self.Owner<>frmSettings then
      SongSelection.frmSongs.UpdateSongPositionInLbxSSelected;
    ShowMeta;
    lblMeta.Top := self.Height-lblMeta.Height-lblMeta.Left;
    if self.Owner<>frmSettings then;
      frmSongs.ImageUpdater.Enabled:=True;
end;

procedure TfrmPresent.ShowMeta;
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
  if Owner <> frmSettings then
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
  end;
  if (Assigned(SlideList)) and (self.Owner = frmSongs) then SlideList.Free;
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
  imgBackground.Width:=self.Width;
  imgBackground.Height:=self.Height;
  if (imgBackground.Height = 0) or (imgBackground.Picture.Height = 0) then Exit; // Prevent Errors
  if imgBackground.Width/imgBackground.Height >= imgBackground.Picture.Width/imgBackground.Picture.Height then
        begin
          newHeight:=Trunc(imgBackground.Width*imgBackground.Picture.Height/imgBackground.Picture.Width);
          imgBackground.Top:=-Abs(Trunc((imgBackground.Height-self.Height)/2));
          imgBackground.Left:=0;
          imgBackground.Height := newHeight;
        end
      else
        begin
          if frmSettings.cbShowBackgroundImage.Checked then // This is important because else there will be a range check error!
          begin
            newWidth:=Trunc(self.Height*imgBackground.Picture.Width/imgBackground.Picture.Height);
            imgBackground.Left:=-Abs(Trunc((imgBackground.Width-self.Width)/2));
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

function ImageOfWinControl(aWinControl: TWinControl): TBitmap;
var cv: TCanvas;
    Src: THackWinControl;
    Img: TImage;
    ImgBmp: TBitmap;
    notUsed: HWND;
    SrcR, DestR: TRect;
begin
  notUsed:=0;
  cv:=TCanvas.Create;
  Img:=TImage.Create(nil);
  ImgBmp:=Img.Picture.Bitmap;
  Src:=THackWinControl(aWinControl);  // damit .GetDeviceContext() sichtbar wird
  try
    cv.Handle:=Src.GetDeviceContext({var}notUsed);  // w�re in Windows gleichbedeutend mit: cv.Handle:=GetDC(self.PageControl.Handle);
    SrcR:=Rect(0,0,Src.Width,Src.Height);
    DestR:=SrcR;
    ImgBmp.PixelFormat:=pf24Bit;
    ImgBmp.Width:=Src.Width;
    ImgBmp.Height:=Src.Height;
    Img.Canvas.CopyRect(DestR,cv,SrcR);
    FreeAndNIL(cv);
    Result := ImgBmp;
  finally
    Img.Free;
    cv.Free;
  end;
end;

end.
