unit PresentationCanvas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Slides, LCLType, LCLIntf, graphtype,
  intfgraphics, Math, LazCanvas,
  StrUtils, // for SplitString
  fpImage, PresentationModels, BGRABitmap, BGRABitmapTypes, BGRAGraphics, Graphics,
  languages;

type


  TPresentationCanvasHandler = class
  public
    SlideSettings: TSlideSettings;
    PresentationStyleSettings: TPresentationStyleSettings;
    Width, Height: Integer;
    Bitmap: TBGRABitmap;
    AdjustedBackgroundPicture: TBGRABitmap;
    ResizedBackgroundBitmap: TBGRABitmap;
    constructor Create; overload;
    constructor Create(aPresentationStyleSettings: TPresentationStyleSettings;
      aSlideSettings: TSlideSettings); overload;
    destructor Destroy; override;
    // Will be used to adjust the brightness of the background
    procedure AdjustTransparency;
    procedure LoadBackgroundBitmap;
    procedure ResizeBackgroundBitmap;
    function PaintSlide(Slide: TSlide): TBGRABitmap;
  private
    BackgroundPicture: TBGRABitmap;
    { 1-slot cache for custom per-slide background images }
    FCustomBgPath:         String;
    FCustomBgColor:        TColor;
    FCustomBgTransparency: Integer;
    FCustomBgResized:      TBGRABitmap;
    function CalculateTextHeight(Font: TFont; RectWidth: Integer;
      TextString: String): Integer;
    procedure AssignBGRAFont(Font: TFont);
    procedure LoadAndResizeCustomBg(const AStyle: TPresentationStyleSettings);
  end;

  { Returns a wordwrapped string of a given string. }
  function GetWordWrappedString(
    Text: String;
    FontName: String;
    FontHeight: Integer;
    FontStyle: BGRAGRAPHICS.TFontStyles;
    MaxWidth: Integer): String;

const
  PADDING: Integer = 15;
  MINSPOILERDISTANCE: Integer = 10;
  MORELYRICSINDICATOR: String = '...';

implementation

function GetWordWrappedString(Text: String; FontName: String;
  FontHeight: Integer; FontStyle: BGRAGRAPHICS.TFontStyles; MaxWidth: Integer): String;
var
  BGRABitmap: TBGRABitmap;
  LetterIndex: Integer;
  CurrentWord: String;
  CurrentLine: String;
  CurrentLetter: String;
begin
  BGRABitmap := TBGRABitmap.Create;
  BGRABitmap.SetSize(MaxWidth, 1000);
  BGRABitmap.FontName:=FontName;
  BGRABitmap.FontHeight:=FontHeight;
  BGRABitmap.FontStyle:=FontStyle;
  BGRABitmap.FontAntialias:=False;
  BGRABitmap.FontQuality:=fqFineAntialiasing;

  CurrentWord := '';
  CurrentLine := '';
  Result := '';

  for LetterIndex := 1 to Length(Text) do
  begin
    CurrentLetter := Text[LetterIndex];

    if (CurrentLetter = ' ') or (CurrentLetter = LineEnding) or (LetterIndex = Length(Text)) then
    begin
      // Test whether the text and the current word would still fit
      if BGRABitmap.TextSize(Trim(CurrentLine + CurrentWord + CurrentLetter), MaxWidth).Width < MaxWidth then
      begin
        // Add the word
        CurrentLine := CurrentLine + CurrentWord + CurrentLetter;
        CurrentWord := '';
        if CurrentLetter <> ' ' then
        begin
          Result := Result + CurrentLine;
          CurrentLine := '';
        end;
      end
      // Text does not fit on the line anymore -> perform a line break
      else
      begin
        Result := Result + CurrentLine + LineEnding + CurrentWord + CurrentLetter;
        CurrentWord := '';
        CurrentLine := '';
      end;
    end
    else
    begin
      CurrentWord := CurrentWord + CurrentLetter;
    end;
  end;
  BGRABitmap.Destroy;
end;

procedure TPresentationCanvasHandler.AdjustTransparency;
var
  x, y: Integer;
  p: PBGRAPixel;
  TransparentBackgroundImage: TBGRABitmap;
begin

  if PresentationStyleSettings.ShowBackgroundImage = false
    then Exit;

  if PresentationStyleSettings.Transparency = 0 then
  begin
    AdjustedBackgroundPicture.Assign(BackgroundPicture);
    Exit;
  end;

  AdjustedBackgroundPicture.SetSize(BackgroundPicture.Width, BackgroundPicture.Height);
  AdjustedBackgroundPicture.FillRect(0,0,
                                         AdjustedBackgroundPicture.Width-1,
                                         AdjustedBackgroundPicture.Height-1,
                                         ColorToBGRA(PresentationStyleSettings.BackgroundColor)
                                     );

  TransparentBackgroundImage := TBGRABitmap.Create;
  TransparentBackgroundImage.Assign(BackgroundPicture);

  for y := 0 to TransparentBackgroundImage.Height-1 do
  begin
    p := TransparentBackgroundImage.Scanline[y];
    for x := 0 to TransparentBackgroundImage.Width-1 do
    begin
      p^.alpha := 255-Round(Abs(PresentationStyleSettings.Transparency*2.55));
      inc(p);
    end;
  end;

  AdjustedBackgroundPicture.PutImage(0,0,TransparentBackgroundImage,dmDrawWithTransparency);

  TransparentBackgroundImage.Destroy;
end;

constructor TPresentationCanvasHandler.Create; overload;
begin
  inherited;
  Bitmap := TBGRABitmap.Create;
  Bitmap.CanvasBGRA.TextStyle.Wordbreak:=True;
  Bitmap.CanvasBGRA.TextStyle.SingleLine:=False;

  BackgroundPicture := TBGRABitmap.Create;
  AdjustedBackgroundPicture := TBGRABitmap.Create;
  ResizedBackgroundBitmap := TBGRABitmap.Create;

  FCustomBgPath := '';
  FCustomBgResized := TBGRABitmap.Create;
end;

constructor TPresentationCanvasHandler.Create(aPresentationStyleSettings:
  TPresentationStyleSettings; aSlideSettings: TSlideSettings); overload;
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
  FCustomBgResized.Destroy;
  PresentationModels.DestroyPresentationStyleSettings(Self.PresentationStyleSettings);
  inherited;
end;

procedure TPresentationCanvasHandler.LoadBackgroundBitmap;
begin
  if (PresentationStyleSettings.ShowBackgroundImage) then
  begin
    try
      BackgroundPicture.LoadFromFile(PresentationStyleSettings.BackgroundImageFilePath);
      AdjustTransparency;
      ResizeBackgroundBitmap;
    except
      PresentationStyleSettings.ShowBackgroundImage := False;
    end;
  end;
end;

procedure TPresentationCanvasHandler.ResizeBackgroundBitmap;
var
  DestRect: TRect;
  NewHeight, NewWidth: Integer;
  ResampledAdjustedBackgroundPicture: TBGRABitmap;
begin
  // Always invalidate the per-song custom-bg cache: the canvas size may have
  // changed (FormResize), so any previously cached scaled bitmap is stale.
  FCustomBgPath := '';

  if PresentationStyleSettings.ShowBackgroundImage = false
     then Exit;

  // Prevent a Division by Zero Exception
  if (self.Height = 0) Or (AdjustedBackgroundPicture.Height = 0) Or
    (AdjustedBackgroundPicture.Width = 0) then Exit;

  if self.Width / self.Height >= AdjustedBackgroundPicture.Width /
    AdjustedBackgroundPicture.Height then
  begin
    NewHeight := Ceil(self.Width * AdjustedBackgroundPicture.Height /
      AdjustedBackgroundPicture.Width);
    if AdjustedBackgroundPicture.Height > self.Height then
      DestRect.Top := Max(-(Trunc(
        (AdjustedBackgroundPicture.Height - self.Height) / 2)), 0)
    else
    DestRect.Top := 0;
    DestRect.Left := 0;
    DestRect.Height := Max(newHeight, self.Height);
    DestRect.Width := Ceil(DestRect.Height * AdjustedBackgroundPicture.Width /
      AdjustedBackgroundPicture.Height);
  end

  else
  begin
    newWidth := Ceil(self.Height * AdjustedBackgroundPicture.Width /
      AdjustedBackgroundPicture.Height);
    if AdjustedBackgroundPicture.Width > self.Width then
      DestRect.Left := Max(-(Trunc(
        (AdjustedBackgroundPicture.Width - self.Width) / 2)), 0)
    else
    DestRect.Left := 0;
    DestRect.Top := 0;
    DestRect.Width := Max(newWidth, self.Width);
    DestRect.Height := Ceil(DestRect.Width * AdjustedBackgroundPicture.Height /
      AdjustedBackgroundPicture.Width);
  end;

  ResizedBackgroundBitmap.SetSize(self.Width, self.Height);
  ResizedBackgroundBitmap.Fill(PresentationStyleSettings.BackgroundColor);
  ResampledAdjustedBackgroundPicture := AdjustedBackgroundPicture.Resample(
                                       DestRect.Width, DestRect.Height,
                                       rmSimpleStretch
                                     );
  ResizedBackgroundBitmap.PutImage(DestRect.Left, DestRect.Top,
                                     ResampledAdjustedBackgroundPicture,
                                     dmDrawWithTransparency
                                  );
  ResampledAdjustedBackgroundPicture.Free;
end;

procedure TPresentationCanvasHandler.LoadAndResizeCustomBg(
  const AStyle: TPresentationStyleSettings);
var
  CustomBgPicture, CustomAdjusted, ResampledBitmap: TBGRABitmap;
  x, y: Integer;
  p: PBGRAPixel;
  DestRect: TRect;
  NewHeight, NewWidth: Integer;
begin
  FCustomBgPath := '';  // reset so failed loads don't reuse stale path
  FCustomBgResized.SetSize(0, 0);
  try
    CustomBgPicture := TBGRABitmap.Create;
    CustomAdjusted   := TBGRABitmap.Create;
    try
      CustomBgPicture.LoadFromFile(AStyle.BackgroundImageFilePath);

      // Adjust transparency
      if AStyle.Transparency = 0 then
        CustomAdjusted.Assign(CustomBgPicture)
      else
      begin
        CustomAdjusted.SetSize(CustomBgPicture.Width, CustomBgPicture.Height);
        CustomAdjusted.FillRect(0, 0, CustomAdjusted.Width-1, CustomAdjusted.Height-1,
          ColorToBGRA(AStyle.BackgroundColor));
        for y := 0 to CustomBgPicture.Height-1 do
        begin
          p := CustomBgPicture.ScanLine[y];
          for x := 0 to CustomBgPicture.Width-1 do
          begin
            p^.alpha := 255 - Round(Abs(AStyle.Transparency * 2.55));
            Inc(p);
          end;
        end;
        CustomAdjusted.PutImage(0, 0, CustomBgPicture, dmDrawWithTransparency);
      end;

      // Resize to fit presentation canvas
      if (Self.Width > 0) and (Self.Height > 0) and
         (CustomAdjusted.Width > 0) and (CustomAdjusted.Height > 0) then
      begin
        if Self.Width / Self.Height >= CustomAdjusted.Width / CustomAdjusted.Height then
        begin
          NewHeight := Ceil(Self.Width * CustomAdjusted.Height / CustomAdjusted.Width);
          if CustomAdjusted.Height > Self.Height then
            DestRect.Top := Max(-(Trunc((CustomAdjusted.Height - Self.Height) / 2)), 0)
          else
            DestRect.Top := 0;
          DestRect.Left := 0;
          DestRect.Height := Max(NewHeight, Self.Height);
          DestRect.Width := Ceil(DestRect.Height * CustomAdjusted.Width / CustomAdjusted.Height);
        end
        else
        begin
          NewWidth := Ceil(Self.Height * CustomAdjusted.Width / CustomAdjusted.Height);
          if CustomAdjusted.Width > Self.Width then
            DestRect.Left := Max(-(Trunc((CustomAdjusted.Width - Self.Width) / 2)), 0)
          else
            DestRect.Left := 0;
          DestRect.Top := 0;
          DestRect.Width := Max(NewWidth, Self.Width);
          DestRect.Height := Ceil(DestRect.Width * CustomAdjusted.Height / CustomAdjusted.Width);
        end;

        FCustomBgResized.SetSize(Self.Width, Self.Height);
        FCustomBgResized.Fill(AStyle.BackgroundColor);
        ResampledBitmap := CustomAdjusted.Resample(DestRect.Width, DestRect.Height,
                                                    rmSimpleStretch);
        FCustomBgResized.PutImage(DestRect.Left, DestRect.Top, ResampledBitmap,
                                  dmDrawWithTransparency);
        ResampledBitmap.Free;

        FCustomBgPath := AStyle.BackgroundImageFilePath;
        FCustomBgColor := AStyle.BackgroundColor;
        FCustomBgTransparency := AStyle.Transparency;
      end;
    finally
      CustomBgPicture.Free;
      CustomAdjusted.Free;
    end;
  except
    // Invalid image / file not found – FCustomBgPath stays '' → no image shown
    FCustomBgResized.SetSize(0, 0);
  end;
end;

function TPresentationCanvasHandler.PaintSlide(Slide: TSlide): TBGRABitmap;
var
  BackgroundRect, ContentRect: TRect;
  MainTextHeight, SpoilerTextHeight, MetaTextHeight: Integer;
  NormalTextFont, SpoilerTextFont, MetaTextFont: TFont;
  SpoilerRectWidth: Integer;
  SpoilerDistance: Integer;
  SpoilerText: String;
  DisplayedMainText: String;
  DefaultSpoilerDistance: Integer;
  TextStyle: TTextStyle;
  EffectiveStyle: TPresentationStyleSettings;
begin
  if Slide.HasCustomStyle then
    EffectiveStyle := Slide.CustomStyle
  else
    EffectiveStyle := PresentationStyleSettings;

  Bitmap.SetSize(self.Width, self.Height);

  // When the slide is empty and black screen mode is enabled, return a pure black bitmap
  if (Slide.SlideType = EmptySlide) and EffectiveStyle.BlackScreenOnEmptySlide then
  begin
    Bitmap.Fill(clBlack);
    Result := Bitmap;
    Exit;
  end;

  Bitmap.Fill(EffectiveStyle.BackgroundColor);
  // Here we setup the different fonts for calculating the text height.
  // Guard against nil Font (e.g. style loaded without font info): fall back
  // to the global presentation style so rendering never crashes.
  if not Assigned(EffectiveStyle.Font) then
    EffectiveStyle.Font := PresentationStyleSettings.Font;
  NormalTextFont := TFont.Create;
  NormalTextFont.Assign(EffectiveStyle.Font);
  NormalTextFont.Color := EffectiveStyle.TextColor;
  DefaultSpoilerDistance := Round(NormalTextFont.GetTextHeight('gJ') * 3.5);
  SpoilerTextFont := TFont.Create;
  SpoilerTextFont.Assign(NormalTextFont);
  SpoilerTextFont.Height := NormalTextFont.Height Div 2;
  SpoilerRectWidth := Round((self.Width - EffectiveStyle.Padding.Left -
    EffectiveStyle.Padding.Right) * 2 / 3);
  MetaTextFont := TFont.Create;
  MetaTextFont.Assign(NormalTextFont);
  MetaTextFont.Height := NormalTextFont.Height Div 3;

  with BackgroundRect do
  begin
    Left := 0;
    Top := 0;
    Width := self.Width;
    Height := self.Height;
  end;

  DisplayedMainText := Slide.PartContent.MainText;

  MainTextHeight := self.CalculateTextHeight(NormalTextFont, self.Width -
    EffectiveStyle.Padding.Left - EffectiveStyle.Padding.Right,
    DisplayedMainText);
  MetaTextHeight := CalculateTextHeight(MetaTextFont, SpoilerRectWidth,
    Slide.PartContent.MetaText);

  SpoilerText := Slide.PartContent.SpoilerText;
  if SpoilerText <> '' then
  begin
    Self.AssignBGRAFont(SpoilerTextFont);
    SpoilerTextHeight := self.CalculateTextHeight(SpoilerTextFont,
      SpoilerRectWidth, SpoilerText);
    // Check whether spoiler fits
    SpoilerDistance := Min(DEFAULTSPOILERDISTANCE, Height -
      (EffectiveStyle.Padding.Top + EffectiveStyle.Padding.Bottom +
      MainTextHeight + SpoilerTextHeight + DEFAULTSPOILERDISTANCE -
      MINSPOILERDISTANCE - MetaTextHeight));

    if SpoilerDistance < DEFAULTSPOILERDISTANCE then
    begin
      SpoilerText := SplitString(SpoilerText, LineEnding)[0] + MoreLyricsIndicator;
      // Now we calculate the height again
      SpoilerTextHeight := self.CalculateTextHeight(SpoilerTextFont,
        self.Width - EffectiveStyle.Padding.Left -
        EffectiveStyle.Padding.Right, SpoilerText);
      SpoilerDistance := (Height - (EffectiveStyle.Padding.Top +
        EffectiveStyle.Padding.Bottom + MainTextHeight +
        2 * SpoilerTextHeight - MetaTextHeight)) Div 2;
      if SpoilerDistance < MINSPOILERDISTANCE then
      begin
        SpoilerText := '';
        SpoilerTextHeight := 0;
        SpoilerDistance := 0;
      end;
    end;
  end

  else
  begin
    SpoilerTextHeight := 0;
    SpoilerDistance := 0;
  end;

  //Insert Background
  if EffectiveStyle.ShowBackgroundImage then
  begin
    if (not Slide.HasCustomStyle) or
       ((Slide.CustomStyle.BackgroundImageFilePath =
         PresentationStyleSettings.BackgroundImageFilePath) and
        (Slide.CustomStyle.BackgroundColor = PresentationStyleSettings.BackgroundColor) and
        (Slide.CustomStyle.Transparency = PresentationStyleSettings.Transparency)) then
      Bitmap.PutImage(0, 0, Self.ResizedBackgroundBitmap, dmSet)
    else
    begin
      if (FCustomBgPath <> EffectiveStyle.BackgroundImageFilePath) or
         (FCustomBgColor <> EffectiveStyle.BackgroundColor) or
         (FCustomBgTransparency <> EffectiveStyle.Transparency) then
        LoadAndResizeCustomBg(EffectiveStyle);
      if FCustomBgResized.Width > 0 then
        Bitmap.PutImage(0, 0, FCustomBgResized, dmSet);
    end;
  end;

  with TextStyle do
  begin
    case EffectiveStyle.HorizontalAlign of
      Align_Left: Alignment := taLeftJustify;
      Align_Center: Alignment := taCenter;
      Align_Right: Alignment := taRightJustify;
    end;
    Layout := tlTop;
    SingleLine := False;
    WordBreak := True;
    Opaque := False;
    EndEllipsis := False;
    RightToLeft := IsRTLLanguage(DisplayedMainText);
  end;

  Self.AssignBGRAFont(NormalTextFont);

  with ContentRect do
  begin
    Left := EffectiveStyle.Padding.Left;
    case EffectiveStyle.VerticalAlign of
      tlTop: Top := EffectiveStyle.Padding.Top;
      tlCenter: Top := EffectiveStyle.Padding.Top +
          (self.Height - EffectiveStyle.Padding.Top -
          EffectiveStyle.Padding.Bottom - MainTextHeight -
          SpoilerTextHeight - SpoilerDistance) Div 2;
      tlBottom: Top := self.Height - EffectiveStyle.Padding.Bottom -
          MainTextHeight - SpoilerTextHeight - SpoilerDistance;
    end;
    Width := self.Width - EffectiveStyle.Padding.Right -
      EffectiveStyle.Padding.Left;
    Height := MainTextHeight;
  end;

  { Make the Title bold }
  if Slide.SlideType = TitleSlide then
    Bitmap.FontStyle += [fsBold];

  { Painting the main text }

  {$if defined(DEBUGCANVAS)}
  { When the DEBUGCANVAS flag is set, we show some additional information for the debugging of the painting }
  Bitmap.Rectangle(ContentRect, clRed);
  Bitmap.Rectangle(ContentRect.Left, ContentRect.Top,
                                     ContentRect.Left+Bitmap.TextSize(DisplayedMainText, ContentRect.Width).Width,
                                     ContentRect.Top+Bitmap.TextSize(DisplayedMainText, ContentRect.Width).Height,
                                     clYellow);
  {$endif}
  Bitmap.TextRect(ContentRect, ContentRect.left, ContentRect.Top, DisplayedMainText, TextStyle,
                               ColorToBgra(EffectiveStyle.TextColor));

  // Repeat Assignment because we changed it to bold before
  Self.AssignBGRAFont(NormalTextFont);

  { Paint the spoiler if desired }
  if SpoilerText <> '' then
  begin
    Self.AssignBGRAFont(SpoilerTextFont);
    ContentRect.Top += MainTextHeight + SpoilerDistance;
    ContentRect.Height := SpoilerTextHeight;
    with TextStyle do
    begin
      RightToLeft := IsRTLLanguage(Slide.PartContent.SpoilerText);
    end;
    Bitmap.TextRect(ContentRect, SpoilerText, TextStyle.Alignment, tlCenter,
                      ColorToBgra(EffectiveStyle.TextColor)
                    );
  end;

  // We paint Meta information if desired
  if Slide.PartContent.MetaText <> '' then
  begin
    ContentRect.Top := self.Height - EffectiveStyle.Padding.Bottom -
      MetaTextHeight;
    ContentRect.Left := EffectiveStyle.Padding.Left;
    ContentRect.Height := MetaTextHeight;
    ContentRect.Width := SpoilerRectWidth;
    with TextStyle do
    begin
      Alignment := taLeftJustify;
      Layout := tlBottom;
      RightToLeft := IsRTLLanguage(Slide.PartContent.MetaText);
    end;
    Self.AssignBGRAFont(MetaTextFont);
    Bitmap.TextRect(ContentRect, Slide.PartContent.MetaText, TextStyle.Alignment, tlTop,
                               ColorToBgra(EffectiveStyle.TextColor));
  end;

  {$if defined(DEBUGCANVAS)}
  { When the DEBUGCANVAS flag is set, we show some additional information for the debugging of the painting }
  Bitmap.Rectangle(ContentRect, clRed);
  {$endif}

  NormalTextFont.Destroy;
  SpoilerTextFont.Destroy;
  MetaTextFont.Destroy;
  Result := Bitmap;
end;

function TPresentationCanvasHandler.CalculateTextHeight(Font: TFont;
  RectWidth: Integer; TextString: String): Integer;
var
  R: TRect;
begin
  Self.AssignBGRAFont(Font);
  Result := Bitmap.TextSize(TextString, RectWidth).Height;
end;

procedure TPresentationCanvasHandler.AssignBGRAFont(Font: TFont);
begin
  // Issue #24
  If (Font.Name='') or (LowerCase(Font.Name)='default') then
    Font.Name := 'Arial';

  Bitmap.FontName  := Font.Name;
  Bitmap.FontStyle := Font.Style;
  Bitmap.FontHeight:= Round(Font.Height/0.85);
end;



end.
