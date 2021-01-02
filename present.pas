unit Present;

{$mode objfpc}{$H+}

interface

uses
  {$if defined(LINUX)} //Für Linux müssen die GTK-Pakete geladen werden, um Vollbildfunktion zu ermöglichen
  gtk2, gdk2,
  {$endif}
  Classes, LCLType, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Settings, Types, Themes, DefaultTranslator;
type

  { TfrmPresent }

  TfrmPresent = class(TForm)
    lblNext: TLabel;
    lblText: TLabel;
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
    procedure showItem(index: integer);
    procedure SwitchFullScreen;
    procedure SwitchFullScreen(WantFullScreen: Boolean);
    procedure LoadSettings;
  private
    { private declarations }
  public
    { public declarations }
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    ScreenBounds: TRect;
    procedure GoPrevious;
    procedure GoNext;
  end;

var
  frmPresent: TfrmPresent;
  textList: TStringList;
  cur: Integer; //The current Index of the String List which is shown
  FullScreen: Boolean;

ResourceString
  strFormCaption = 'Präsentation';

implementation

Uses
  Unit1;
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
end;

procedure TfrmPresent.GoPrevious;
begin
  if (cur > 0) then
  begin
    dec(cur);
    ShowItem(cur);
  end;
end;

procedure TfrmPresent.FormResize(Sender: TObject);
begin
  showItem(cur);
end;

procedure TfrmPresent.FormShow(Sender: TObject);
begin
  LoadSettings;
  if textList.Count>0 then showItem(0) else frmPresent.Hide;
end;

procedure TFrmPresent.LoadSettings;
begin
  frmPresent.Color:=frmSettings.bgColorDialog.Color;
  frmPresent.lblText.Font.Color:=frmSettings.textColorDialog.Color;
  lblText.Font := frmSettings.FontDialog.Font;
  lblText.Font.Color:= frmSettings.textColorDialog.Color;
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
  SwitchFullScreen();
end;


procedure TfrmPresent.showItem(index: integer);
var i,lines, lastposition, pHeight, pWidth: integer;
  longestline: string;
begin
    lblText.WordWrap:=True;
    lblText.Font := frmSettings.FontDialog.Font;
    lblText.Font.Color:= frmSettings.textColorDialog.Color;

    if ((frmSettings.cbSpoiler.Checked) and (textList.Count > cur + 1) and (textList.Strings[cur] <> '')) then
    begin
      lblNext.Visible:=True;
      //lblNext.Caption := copy(textList.Strings[cur+1], 1, pos(LineEnding, textList.Strings[cur+1])-1);
      lblNext.Caption := textList.Strings[cur+1];
      lblNext.Color := lblText.Color;
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
    lines := 0;
    Unit1.frmSongs.ImageUpdater.Enabled:=True;
end;

procedure TfrmPresent.FormCreate(Sender: TObject);
begin
  cur := 0;
  present.textList := TStringList.Create;
  FullScreen := False;
  self.Caption:= strFormCaption;
end;

procedure TfrmPresent.FormDestroy(Sender: TObject);
begin
  textList.Free;
end;

procedure TfrmPresent.FormHide(Sender: TObject);
begin
  // Stelle Unit1 wieder her
  Unit1.ProgrammMode:=Unit1.ModeSelection;
  Unit1.frmSongs.FormResize(self);

  // Aktiviere Präsentations-Button, um Präsentation erneut starten zu können

  unit1.frmSongs.itemPresentation.Enabled := True;
  unit1.frmSongs.btnStartPresentation.Enabled := True;

  // Deaktiviere Vollbildschirm (falls noch möglich)

  SwitchFullScreen(False);

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
    gdk_window_fullscreen(PGtkWidget(Handle)^.window);
    Fullscreen := True;
  end else begin
    // From full screen
    gdk_window_unfullscreen(PGtkWidget(Handle)^.window);
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
    gdk_window_fullscreen(PGtkWidget(Handle)^.window);
    Fullscreen := True;
  end else begin
    // From full screen
    gdk_window_unfullscreen(PGtkWidget(Handle)^.window);
    Fullscreen := False;
  end;
  {$endif}
end;

end.

