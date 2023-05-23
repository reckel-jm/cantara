unit PresentationCanvas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Slides, LCLType, LCLIntf, Graphics, IntfGraphics, Math,
  StrUtils, // for SplitString
  fpImage;

type
  TPresentationStyleSettings = record
    BackgroundColor: TColor;
    Font: TFont;
    TextColor: TColor;
    ShowBackgroundImage: Boolean;
    BackgroundImageFilePath: String;
    Transparency: Integer;
  end;

  TPresentationCanvasHandler = class
    public
      SlideSettings: TSlideSettings;
      PresentationStyleSettings: TPresentationStyleSettings;
      Width, Height: Integer;
      Bitmap: TBitmap;
      constructor Create; overload;
      constructor Create(aPresentationStyleSettings: TPresentationStyleSettings; aSlideSettings: TSlideSettings); overload;
      destructor Destroy; override;
      // Will be used to adjust the brightness of the background
      procedure AdjustBrightness(Offset: Integer);
      procedure LoadBackgroundBitmap;
      procedure ResizeBackgroundBitmap;
      function PaintSlide(Slide: TSlide): TBitmap;
    private
      BackgroundPicture: TPicture;
      AdjustedBackgroundPicture: TPicture;
      ResizedBackgroundBitmap: TBitmap;
      function CalculateTextHeight(Font: TFont; RectWidth: Integer; TextString: String): Integer;
  end;

const
  PADDING:integer = 15;
  DEFAULTSPOILERDISTANCE:integer = 20;
  MINSPOILERDISTANCE:Integer = 10;
  MORELYRICSINDICATOR:String = '...';

implementation
procedure TPresentationCanvasHandler.AdjustBrightness(Offset: Integer);
var
  x,y,farbe,Prozent:Integer;
  r,g,b:byte;
begin
  Prozent := PresentationStyleSettings.Transparency;
  AdjustedBackgroundPicture.Clear;
  // Here we create the necessary structures
  AdjustedBackgroundPicture.Assign(BackgroundPicture);
  if Prozent<0 then begin                      //Wenn abdunkeln
    for y:=0 to AdjustedBackgroundPicture.Height-1 do
      for x:=0 to AdjustedBackgroundPicture.Width-1 do begin
        farbe:=AdjustedBackgroundPicture.Bitmap.Canvas.Pixels[x,y];
        b:=byte(farbe shr 16);                 //b=Blau (0..255 oder $00..$FF)
        g:=byte(farbe shr 8);                  //g=Grün (0..255 oder $00..$FF)
        r:=byte(farbe);                        //r=rot(0..255 oder $00..$FF)
        r:=round(r*(100+prozent)/100);
        g:=round(g*(100+prozent)/100);
        b:=round(b*(100+prozent)/100);
        AdjustedBackgroundPicture.Bitmap.Canvas.Pixels[x,y]:=b shl 16 + g shl 8 + r;
      end;
  end else begin                               //ansonsten aufhellen
    for y:=0 to AdjustedBackgroundPicture.Height-1 do
      for x:=0 to AdjustedBackgroundPicture.Width-1 do begin
        farbe:=AdjustedBackgroundPicture.Bitmap.Canvas.Pixels[x,y];
        b:=byte(farbe shr 16);
        g:=byte(farbe shr 8);
        r:=byte(farbe);
        r:=round(r+((255-r)*(prozent)/100));
        g:=round(g+((255-g)*(prozent)/100));
        b:=round(b+((255-b)*(prozent)/100));
        AdjustedBackgroundPicture.Bitmap.Canvas.Pixels[x,y]:=b shl 16 + g shl 8 + r;
      end;
  end;
end;

constructor TPresentationCanvasHandler.Create; overload;
begin
  inherited;
  Bitmap := TBitmap.Create;
  Bitmap.PixelFormat:= pf32Bit;
  BackgroundPicture := TPicture.Create;
  ResizedBackgroundBitmap := TBitmap.Create;
  ResizedBackgroundBitmap.PixelFormat:= pf32Bit;
  AdjustedBackgroundPicture := TPicture.Create;
end;

constructor TPresentationCanvasHandler.Create(aPresentationStyleSettings: TPresentationStyleSettings; aSlideSettings: TSlideSettings); overload;
begin
  Create;
  PresentationStyleSettings := aPresentationStyleSettings;
  SlideSettings := aSlideSettings;
  LoadBackgroundBitmap;
end;

destructor TPresentationCanvasHandler.Destroy;
begin
  Bitmap.Destroy;
  BackgroundPicture.Destroy;
  ResizedBackgroundBitmap.Destroy;
  AdjustedBackgroundPicture.Destroy;
  inherited;
end;

procedure TPresentationCanvasHandler.LoadBackgroundBitmap;
begin
  if PresentationStyleSettings.ShowBackgroundImage then
  begin
    BackgroundPicture.Clear;
    BackgroundPicture.LoadFromFile(PresentationStyleSettings.BackgroundImageFilePath);
    AdjustBrightness(PresentationStyleSettings.Transparency);
    //AdjustedBackgroundPicture.Assign(BackgroundPicture);
    ResizeBackgroundBitmap;
  end;
end;

procedure TPresentationCanvasHandler.ResizeBackgroundBitmap;
var
  DestRect: TRect;
  NewHeight, NewWidth: Integer;
begin
  ResizedBackgroundBitmap.Clear;
  if self.Width/self.Height >= AdjustedBackgroundPicture.Width/AdjustedBackgroundPicture.Height then
  begin
    NewHeight:=Trunc(self.Width*AdjustedBackgroundPicture.Height/AdjustedBackgroundPicture.Width);
    DestRect.Top:=-Abs(Trunc((AdjustedBackgroundPicture.Height-self.Height)/2));
    DestRect.Left:=0;
    DestRect.Height := newHeight;
    DestRect.Width := Trunc(newHeight*AdjustedBackgroundPicture.Width/AdjustedBackgroundPicture.Height);
  end else
  begin
    newWidth:=Trunc(self.Height*AdjustedBackgroundPicture.Width/AdjustedBackgroundPicture.Height);
    DestRect.Left:=-Abs(Trunc((AdjustedBackgroundPicture.Width-self.Width)/2));
    DestRect.Top:=0;
    DestRect.Width:=newWidth;
    DestRect.Height:=Trunc(newWidth*AdjustedBackgroundPicture.Height/AdjustedBackgroundPicture.Width);
  end;
  ResizedBackgroundBitmap.SetSize(self.Width, self.Height);
  ResizedBackgroundBitmap.Canvas.StretchDraw(DestRect, AdjustedBackgroundPicture.Bitmap);
end;

function TPresentationCanvasHandler.PaintSlide(Slide: TSlide): TBitmap;
var
  BackgroundRect, ContentRect: TRect;
  MainTextHeight, SpoilerTextHeight, MetaTextHeight: Integer;
  NormalTextFont, SpoilerTextFont, MetaTextFont: TFont;
  SpoilerRectWidth: Integer;
  SpoilerDistance: Integer;
  SpoilerText: String;
begin
  Bitmap.Clear;
  Bitmap.SetSize(self.Width, self.Height);
  // Here we setup the different fonts for calculating the text height
  NormalTextFont := TFont.Create;
  NormalTextFont.Assign(PresentationStyleSettings.Font);
  NormalTextFont.Color:=PresentationStyleSettings.TextColor;
  SpoilerTextFont := TFont.Create;
  SpoilerTextFont.Assign(NormalTextFont);
  SpoilerTextFont.Height:=NormalTextFont.Height div 2;
  MetaTextFont := TFont.Create;
  MetaTextFont.Assign(NormalTextFont);
  MetaTextFont.Height:=NormalTextFont.Height div 3;
  with BackgroundRect do
  begin
    Left := 0;
    Top := 0;
    Width := self.Width;
    Height := self.Height;
  end;
  MainTextHeight := self.CalculateTextHeight(NormalTextFont, self.Width-2*Padding, Slide.PartContent.MainText);
  MetaTextHeight := CalculateTextHeight(MetaTextFont, SpoilerRectWidth, Slide.PartContent.MetaText);
  SpoilerText := Slide.PartContent.SpoilerText;
  if SpoilerText <> '' then
  begin
    SpoilerTextHeight := self.CalculateTextHeight(SpoilerTextFont, self.Width-2*Padding, SpoilerText);
    SpoilerRectWidth := Round((self.Width-2*Padding)*2/3);
    // Check whether spoiler fits
    SpoilerDistance := Min(DEFAULTSPOILERDISTANCE, Height-(2*Padding+MainTextHeight+SpoilerTextHeight+DEFAULTSPOILERDISTANCE-MINSPOILERDISTANCE-MetaTextHeight));
    if SpoilerDistance < DEFAULTSPOILERDISTANCE then
    begin
      SpoilerText := SplitString(SpoilerText, LineEnding)[0] + MoreLyricsIndicator;
      // Now we calculate the height again
      SpoilerTextHeight := self.CalculateTextHeight(SpoilerTextFont, self.Width-2*Padding, SpoilerText);
      SpoilerDistance := Min(MINSPOILERDISTANCE, Height-(2*Padding+MainTextHeight+SpoilerTextHeight+DEFAULTSPOILERDISTANCE-MINSPOILERDISTANCE-MetaTextHeight));
      if SpoilerDistance < MINSPOILERDISTANCE then
      begin
        SpoilerText := '';
        SpoilerTextHeight := 0;
      end;
    end;
  end
  else
  begin
    SpoilerTextHeight := 0;
    SpoilerDistance := 0;
  end;
  with Bitmap.Canvas do
  begin
    Brush.Color := PresentationStyleSettings.BackgroundColor;
    Brush.Style := bsSolid;
    FillRect(Bitmap.Canvas.ClipRect);
    FillRect(BackgroundRect);
    //Insert Background
    if PresentationStyleSettings.ShowBackgroundImage then
    begin
      BitBlt(Bitmap.Canvas.Handle, 0, 0, self.Width, self.Height, ResizedBackgroundBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    end;
    with TextStyle do
    begin
      Alignment:= taCenter;
      Layout:= tlTop;
      SingleLine := False;
      WordBreak := True;
      Opaque := False;
    end;
    Font.Assign(NormalTextFont);
    with ContentRect do
    begin
      Left := PADDING;
      Top := Max(Padding, Padding+(self.Height-2*Padding-MainTextHeight-SpoilerTextHeight-2*SpoilerDistance) div 2);
      Width := self.Width-Padding;
      Height := MainTextHeight;
    end;
    TextRect(ContentRect, ContentRect.Left, ContentRect.Top, Slide.PartContent.MainText);
    // We paint the spoiler if desired
    if SpoilerText <> '' then
    begin
      Font.Assign(SpoilerTextFont);
      ContentRect.Top += MainTextHeight + SpoilerDistance;
      ContentRect.Height:=SpoilerTextHeight;
      TextRect(ContentRect, ContentRect.Left, ContentRect.Top, SpoilerText);
    end;
    // We paint Meta information if desired
    if Slide.PartContent.MetaText <> '' then
    begin
      ContentRect.Top := self.Height-Padding-MetaTextHeight;
      ContentRect.Left:=Padding;
      ContentRect.Height:=MetaTextHeight;
      ContentRect.Width:=SpoilerRectWidth;
      with TextStyle do
        Alignment := taLeftJustify;
      Font.Assign(MetaTextFont);
      TextRect(ContentRect, ContentRect.Left, ContentRect.Top, Slide.PartContent.MetaText);
    end;
  end;
  //Bitmap.SaveToFile(GetTempDir() + 'foto.png');
  NormalTextFont.Destroy;
  SpoilerTextFont.Destroy;
  MetaTextFont.Destroy;
  Result := Bitmap;
end;

function TPresentationCanvasHandler.CalculateTextHeight(Font: TFont; RectWidth: Integer; TextString: String): Integer;
var
  R: TRect;
begin
  Bitmap.Canvas.Font.Assign(Font);
  R  := Rect(0, 0, RectWidth, 0);
  Result := DrawText
    (Bitmap.Canvas.Handle, PChar(TextString), Length(TextString), R, dt_CalcRect Or dt_WordBreak);
end;

end.

