unit presentationcontroller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls;

type

  { TFramePresentationController }

  TFramePresentationController = class(TFrame)
    ButtonCancel: TButton;
    ButtonPrevious: TButton;
    ButtonNext: TButton;
    CurrentImage: TImage;
    PageControls: TPageControl;
    ButtonPanal: TPanel;
    Splitter: TSplitter;
    PageText: TTabSheet;
    TabSheet1: TTabSheet;
  private

  public

  end;

implementation

{$R *.lfm}

end.

