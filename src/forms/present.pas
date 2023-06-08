unit Present;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Types, Themes, LCLTranslator, LCLIntf, ExtCtrls, Lyrics,
  IntfGraphics,
  fpImage, StrUtils, Slides,
  math,
  PresentationCanvas;
type

  THackWinControl = class(TWinControl);

  { TfrmPresent }

  TfrmPresent = class(TForm)
    imageShower: TImage;
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
    procedure imageShowerClick(Sender: TObject);
    procedure showItem(index: integer);
    procedure SwitchFullScreen;
    procedure SwitchFullScreen(WantFullScreen: Boolean);
    procedure ShowMeta;
  private
    { private declarations }
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
    procedure GoPrevious;
    procedure GoNext;
    procedure Refresh;
    procedure ShowFirst;
  end;

{ We need to overload in here with Word, so it can be used to determine if a pressed key is in one
of the constant arrays defined below. }
operator in (const AWord: Word; const AArray: array of Word): Boolean; inline;

const
  { Here we define the list of keys which can be used to move to the next slide (GoRightKeys), go to the previous slide (GoLeftKeys,
  toggle fullscreen (ToggleFullscreenKeys) or quit the presentation (EscapeKeys). }
  GoRightKeys: array[0..6] of Word = (VK_RIGHT, VK_DOWN, VK_SPACE, VK_RETURN, VK_MEDIA_NEXT_TRACK, VK_BROWSER_FORWARD, VK_NEXT);
  GoLeftKeys: array[0..4] of Word = (VK_LEFT, VK_UP, VK_MEDIA_PREV_TRACK, VK_BROWSER_BACK, VK_PRIOR);
  ToggleFullscreenKeys: array[0..1] of Word = (VK_F11, VK_F5);
  EscapeKeys: array of Word = (VK_Escape);

var
  frmPresent: TfrmPresent;

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
  PresentationCanvas.Width:=self.Width;
  PresentationCanvas.Height:=self.Height;
  PresentationCanvas.ResizeBackgroundBitmap;
  showItem(cur);
end;

procedure TfrmPresent.FormShow(Sender: TObject);
begin
  PresentationCanvas.LoadBackgroundBitmap;
  if SlideList.Count >0 then showItem(0) else self.Hide;
  Refresh;
end;

procedure TfrmPresent.imageShowerClick(Sender: TObject);
begin
  self.GoNext;
end;

procedure TfrmPresent.showItem(index: integer);
var SlideBitmap: TBitmap; // StringArray: TStringDynArray;
begin
  cur := index;
  SlideBitmap := PresentationCanvas.PaintSlide(SlideList.Items[cur]);
  imageShower.Left := 0;
  imageShower.Top:=0;
  imageShower.Picture.Bitmap.Assign(SlideBitmap);
end;

procedure TfrmPresent.ShowMeta;
begin

end;

procedure TfrmPresent.FormCreate(Sender: TObject);
begin
  cur := 0;
  FullScreen := False;
  self.WindowState:= wsMaximized;
  PresentationCanvas := TPresentationCanvasHandler.Create;
  self.SlideList := TSlideLIst.Create(True);
end;

procedure TfrmPresent.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TfrmPresent.FormDestroy(Sender: TObject);
begin
  SlideList.Destroy;
  PresentationCanvas.Destroy;
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

end.
