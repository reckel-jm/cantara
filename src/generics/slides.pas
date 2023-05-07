unit slides;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Lyrics, fgl;

type

  { An Element of every slide which represents its text content. }
  TPartContent = class
    public
      MainText: String;
      SpoilerText: String;
      MetaText: String;
      constructor Create; overload;
  end;

  { Represents an abstraction of a slide to be shown. }
  TSlide = class
    public
      Song: TSong;
      PartContent: TPartContent;
      Extra: String;
      ID: String;
      constructor Create; overload;
      destructor Destroy; override;

  end;

  TSlideList = specialize TFPGObjectList<TSlide>;

implementation
  constructor TPartContent.Create;
  begin
    inherited;
    MainText := '';
    SpoilerText := '';
    MetaText := '';
  end;

  constructor TSlide.Create;
  begin
    inherited;
    self.PartContent := TPartContent.Create;
  end;

  destructor TSlide.Destroy;
  begin
    FreeAndNil(self.PartContent);
    inherited;
  end;

end.

