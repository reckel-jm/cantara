{ Unit for handling the logic of the slides in Cantara

  Copyright (C) 2023 Jan Martin Reckel

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
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

