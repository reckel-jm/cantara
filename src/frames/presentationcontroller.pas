unit presentationcontroller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls, Slides;

type
  { TOnChangeSlide
    The event which will be raised if a slide change has been commited by the user
  }
  TOnChangeSlide = procedure(NewSlideNumber: Integer) of object;

  { TFramePresentationController }

  TFramePresentationController = class(TFrame)
    Button1: TButton;
    ButtonCancel: TButton;
    ButtonPrevious: TButton;
    ButtonNext: TButton;
    CurrentImage: TImage;
    SongListBox: TListBox;
    SongListBoxContainer: TGroupBox;
    PageControls: TPageControl;
    ButtonPanal: TPanel;
    Splitter1: TSplitter;
    VerticalSplitter: TSplitter;
    PageText: TTabSheet;
    TabSheet1: TTabSheet;
  private
    procedure ChangeCurrentSlide(NewSlideNumber: Integer);
    fCurrentSlide: Integer;
  public
    property CurrentSlide: Integer read fCurrentSlide write ChangeCurrentSlide;
    OnChangeSlide: TOnChangeSlide;
    Slides: TSlideList;

  end;

implementation

{$R *.lfm}

{ TFramePresentationController }

procedure TFramePresentationController.ChangeCurrentSlide(
  NewSlideNumber: Integer);
begin
  { To Add: The design changes }
  Self.fCurrentSlide:=NewSlideNumber;
end;

end.

