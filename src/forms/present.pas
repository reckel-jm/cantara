unit Present;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Types, Themes, LCLTranslator, LCLIntf, ExtCtrls, Lyrics,
  IntfGraphics, Menus,
  fpImage, StrUtils, Slides,
  Math,
  PresentationCanvas, bgrabitmap, BGRABitmapTypes, presentationcontroller;

type

  THackWinControl = class(TWinControl);

  { TfrmPresent }

  TfrmPresent = class(TForm)
    imageShower: TImage;
    ManipulatedBitmap: TBitmap;
    MenuItemQuitPresentation: TMenuItem;
    MenuItemMoveToScreen: TMenuItem;
    MenuItemToggleFullScreen: TMenuItem;
    PresentationMenu: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imageShowerClick(Sender: TObject);
    procedure MenuItemMoveToScreenClick(Sender: TObject);
    procedure MenuItemQuitPresentationClick(Sender: TObject);
    procedure MenuItemToggleFullScreenClick(Sender: TObject);
    procedure showItem(index: Integer);
    procedure SwitchFullScreen;
    procedure SwitchFullScreen(WantFullScreen: Boolean);
  private
    { private declarations }
    ConnectedController: IPresentationController;
    FadePreviousBitmap: TBGRABitmap;
    FadeNextBitmap: TBGRABitmap;
    FadeBlendBitmap: TBGRABitmap;
    FadeStep: Integer;
    FadeTimer: TTimer;
    procedure OnFadeTimer(Sender: TObject);
    function getCurrentSong: lyrics.TSong;
  public
    { public declarations }
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    ScreenBounds: TRect;
    SlideList: TSlideList;
    cur: Integer; //The current Index of the String List which is shown
    FullScreen: Boolean;
    PresentationCanvas: TPresentationCanvasHandler;
    SlideBitmap: TBGRABitmap;
    procedure GoPrevious;
    procedure GoNext;
    procedure Refresh;
    procedure ShowFirst;
  end;

{ We need to overload in here with Word, so it can be used to determine if a pressed key is in one
of the constant arrays defined below. }
operator In (const AWord: Word; const AArray: array of Word): Boolean; inline;

const
  FADE_STEPS       = 10;  { number of blend frames }
  FADE_INTERVAL_MS = 25;  { ms per frame → 250 ms total transition }

  { Here we define the list of keys which can be used to move to the next slide (GoRightKeys), go to the previous slide (GoLeftKeys,
  toggle fullscreen (ToggleFullscreenKeys) or quit the presentation (EscapeKeys). }
  GoRightKeys: array[0..6] of Word =
    (VK_RIGHT, VK_DOWN, VK_SPACE, VK_RETURN, VK_MEDIA_NEXT_TRACK,
    VK_BROWSER_FORWARD, VK_NEXT);
  GoLeftKeys: array[0..4] of Word =
    (VK_LEFT, VK_UP, VK_MEDIA_PREV_TRACK, VK_BROWSER_BACK, VK_PRIOR);
  ToggleFullscreenKeys: array[0..1] of Word = (VK_F11, VK_F5);
  EscapeKeys: array of Word = (VK_Escape);

var
  frmPresent: TfrmPresent;

implementation

uses
  SongSelection, Settings;
  {$R *.lfm}

operator In (const AWord: Word; const AArray: array of Word): Boolean; inline;
var
  Item: Word;
begin
  for Item In AArray do
    if Item = AWord then
      Exit(True);
  Result := False;
end;

{ TfrmPresent }

procedure TfrmPresent.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key In GoRightKeys then
    GoNext
  else if key In GoLeftKeys then GoPrevious
  else if key In ToggleFullscreenKeys then SwitchFullscreen()
  else if key In EscapeKeys then self.Hide;
end;

procedure TfrmPresent.GoNext;
begin
  if (cur < self.SlideList.Count - 1) then
  begin
    Inc(cur);
    ShowItem(cur);
  end;
end;

procedure TfrmPresent.GoPrevious;
begin
  if (cur > 0) then
  begin
    Dec(cur);
    ShowItem(cur);
  end;
end;

procedure TfrmPresent.FormResize(Sender: TObject);
begin
  if FadeTimer.Enabled then
  begin
    FadeTimer.Enabled := False;
    FadeStep := 0;
  end;
  PresentationCanvas.Width := self.Width;
  PresentationCanvas.Height := self.Height;
  PresentationCanvas.ResizeBackgroundBitmap;
  showItem(cur);
end;

procedure TfrmPresent.FormShow(Sender: TObject);
begin
  PresentationCanvas.LoadBackgroundBitmap;
  if SlideList.Count > 0 then showItem(0)
  else
    self.Hide;
  Refresh;
end;

procedure TfrmPresent.imageShowerClick(Sender: TObject);
begin
  self.GoNext;
end;

procedure TfrmPresent.MenuItemMoveToScreenClick(Sender: TObject);
var
  ActiveMonitor: TMonitor;
  i, ActiveMonitorIndex, MoveToMonitorIndex: Integer;
  FullScreenWasActive: Boolean;
  BeforeWindowState: TWindowState;
begin
  Screen.UpdateMonitors;
  if Screen.MonitorCount <= 1 then Exit;

  BeforeWindowState := Self.WindowState;

  ActiveMonitor := Screen.MonitorFromWindow(Self.Handle);

  for i := 0 to Screen.MonitorCount -1 do
  begin
    if ActiveMonitor = Screen.Monitors[i] then
    begin
      ActiveMonitorIndex := i;
      Break;
    end;
  end;

  if ActiveMonitorIndex < Screen.MonitorCount-1 then
    MoveToMonitorIndex := ActiveMonitorIndex + 1
  else MoveToMonitorIndex := 0;

  FullScreenWasActive := Self.FullScreen;

  if Self.FullScreen then
  begin
    Self.SwitchFullScreen(False);
    Application.ProcessMessages;
  end;
  Self.Left:=Screen.Monitors[MoveToMonitorIndex].Left;
  Self.Top:=Screen.Monitors[MoveToMonitorIndex].Top;
  Self.Refresh;
  Self.Invalidate;
  Application.ProcessMessages;

  If FullScreenWasActive then
  begin
    Self.SwitchFullScreen(True);
    Application.ProcessMessages;
  end else
    Self.WindowState := BeforeWindowState;
end;

procedure TfrmPresent.MenuItemQuitPresentationClick(Sender: TObject);
begin
  Self.Hide;
end;

procedure TfrmPresent.MenuItemToggleFullScreenClick(Sender: TObject);
begin
  Self.SwitchFullscreen;
end;

procedure TfrmPresent.showItem(index: Integer);
var
  CanFade: Boolean;
begin
  // If a fade is already running, snap to its target and use that as the new baseline
  if FadeTimer.Enabled then
  begin
    FadeTimer.Enabled := False;
    FadeStep := 0;
    FadePreviousBitmap.SetSize(FadeNextBitmap.Width, FadeNextBitmap.Height);
    FadePreviousBitmap.Assign(FadeNextBitmap);
  end;

  cur := index;

  CanFade := PresentationCanvas.PresentationStyleSettings.FadeTransition
             and (FadePreviousBitmap.Width  = Self.Width)
             and (FadePreviousBitmap.Height = Self.Height)
             and (Self.Width > 0) and (Self.Height > 0);

  if CanFade then
  begin
    SlideBitmap := PresentationCanvas.PaintSlide(SlideList.Items[cur]);
    FadeNextBitmap.SetSize(Self.Width, Self.Height);
    FadeBlendBitmap.SetSize(Self.Width, Self.Height);
    FadeNextBitmap.Assign(SlideBitmap);
    FadeTimer.Enabled := True;
  end
  else
  begin
    SlideBitmap := PresentationCanvas.PaintSlide(SlideList.Items[cur]);
    SlideBitmap.InvalidateBitmap;
    ImageShower.Picture.Bitmap.Assign(SlideBitmap.Bitmap);
    if (Self.Width > 0) and (Self.Height > 0) then
    begin
      FadePreviousBitmap.SetSize(Self.Width, Self.Height);
      FadePreviousBitmap.Assign(SlideBitmap);
    end;
    ConnectedController.ReloadPresentationImage;
  end;
end;

procedure TfrmPresent.FormCreate(Sender: TObject);
begin
  cur := 0;
  FullScreen := False;
  self.WindowState := wsMaximized;
  PresentationCanvas := TPresentationCanvasHandler.Create;
  self.SlideList := TSlideLIst.Create(True);

  Self.ConnectedController := frmSongs;

  FadePreviousBitmap := TBGRABitmap.Create;
  FadeNextBitmap     := TBGRABitmap.Create;
  FadeBlendBitmap    := TBGRABitmap.Create;
  FadeStep := 0;
  FadeTimer := TTimer.Create(Self);
  FadeTimer.Enabled  := False;
  FadeTimer.Interval := FADE_INTERVAL_MS;
  FadeTimer.OnTimer  := @OnFadeTimer;
end;

procedure TfrmPresent.FormDestroy(Sender: TObject);
begin
  if FadeTimer.Enabled then FadeTimer.Enabled := False;
  SlideList.Destroy;
  PresentationCanvas.Destroy;
  FadePreviousBitmap.Free;
  FadeNextBitmap.Free;
  FadeBlendBitmap.Free;
end;

procedure TfrmPresent.FormHide(Sender: TObject);
begin
  if FadeTimer.Enabled then
  begin
    FadeTimer.Enabled := False;
    FadeStep := 0;
  end;
  if Owner <> frmSettings then
  begin
    // Stelle frmSongs wieder her
    frmSongs.PresentationHasBeenEnded;
    SongSelection.frmSongs.FormResize(self);
    SongSelection.frmSongs.KeyPreview := False;

    // Aktiviere Präsentations-Button, um Präsentation erneut starten zu können

    SongSelection.frmSongs.itemPresentation.Enabled := True;
    SongSelection.frmSongs.btnStartPresentation.Enabled := True;

    // Deaktiviere Vollbildschirm (falls noch möglich)

    SwitchFullScreen(False);
    frmSongs.UpdateControls;
  end;
  if (Assigned(SlideList)) And (self.Owner = frmSongs) then SlideList.Free;
end;

procedure TfrmPresent.SwitchFullScreen;
{$if defined(WINDOWS)}
var c: Integer;
{$endif}
begin
  {$if defined(WINDOWS)}
  c := cur;
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
  cur := c;
  ShowItem(cur);
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
  MenuItemToggleFullScreen.Checked:=Fullscreen;
end;

procedure TfrmPresent.SwitchFullScreen(WantFullScreen: Boolean);
{$if defined(WINDOWS)}
var c: Integer;
{$endif}
begin
  {$if defined(WINDOWS)}
  c := cur;
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
  cur := c;
  ShowItem(cur);
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
  MenuItemToggleFullScreen.Checked:=Fullscreen;
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

procedure TfrmPresent.OnFadeTimer(Sender: TObject);
var
  prevP, nextP, blendP: PBGRAPixel;
  x, y, t, invT: Integer;
begin
  Inc(FadeStep);
  t    := (FadeStep * 256) div FADE_STEPS;
  invT := 256 - t;

  for y := 0 to FadeBlendBitmap.Height - 1 do
  begin
    prevP  := FadePreviousBitmap.ScanLine[y];
    nextP  := FadeNextBitmap.ScanLine[y];
    blendP := FadeBlendBitmap.ScanLine[y];
    for x := 0 to FadeBlendBitmap.Width - 1 do
    begin
      blendP^.red   := (prevP^.red   * invT + nextP^.red   * t) shr 8;
      blendP^.green := (prevP^.green * invT + nextP^.green * t) shr 8;
      blendP^.blue  := (prevP^.blue  * invT + nextP^.blue  * t) shr 8;
      blendP^.alpha := 255;
      Inc(prevP); Inc(nextP); Inc(blendP);
    end;
  end;

  FadeBlendBitmap.InvalidateBitmap;
  ImageShower.Picture.Bitmap.Assign(FadeBlendBitmap.Bitmap);

  if FadeStep >= FADE_STEPS then
  begin
    FadeTimer.Enabled := False;
    FadeStep := 0;
    FadePreviousBitmap.SetSize(FadeNextBitmap.Width, FadeNextBitmap.Height);
    FadePreviousBitmap.Assign(FadeNextBitmap);
    ConnectedController.ReloadPresentationImage;
  end;
end;

end.
