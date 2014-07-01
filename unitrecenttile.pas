unit unitRecentTile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, StrHolder, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, Dialogs, unitStartFunctions, unitRecordLogMetadata;

type

  { TframeRecentTile }

  TframeRecentTile = class(TFrame)
    imgOverlay: TImage;
    lblFilename: TLabel;
    shapeBG: TShape;
    procedure imgOverlayClick(Sender: TObject);
    procedure imgOverlayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgOverlayMouseEnter(Sender: TObject);
    procedure imgOverlayMouseLeave(Sender: TObject);
    procedure imgOverlayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    FPath : String;
    ColorEnter : TColor;
    ColorLeave : TColor;
    ColorDown : TColor;
    ColorUp : TColor;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); overload;
    property Path: String read FPath;
  published
    procedure setColorEnter(newColor : TColor);
    procedure setColorLeave(newColor : TColor);
    procedure setColorInitial(newColor : TColor);
    procedure setColorDown(newColor : TColor);
    procedure setColorUp(newColor : TColor);
    procedure setFilename(newFilename : String);
    procedure setPath(newPath : String);

  end;

implementation

{$R *.lfm}

{ TframeRecentTile }

procedure TframeRecentTile.imgOverlayClick(Sender: TObject);
begin
  shapeBG.Brush.Color := ColorLeave;

  ////////////////////////
  // Open the file here //
  ////////////////////////


end;

procedure TframeRecentTile.imgOverlayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  shapeBG.Brush.Color := ColorDown;
  lblFilename.Font.Style := [fsItalic];
end;

procedure TframeRecentTile.imgOverlayMouseEnter(Sender: TObject);
var
  memoParentDetails : TMemo;
  outLogMetadata : RLogMetadata;
begin
  shapeBG.Brush.Color := ColorEnter;
  lblFilename.Font.Style := [fsItalic];

  // Since the frame is created at runtime, it can't directly access the
  // parent (formStartDialog) or its components. So we have to search for
  // memoDetails to apply properties.
  memoParentDetails := TMemo(Owner.FindComponent('memoDetails'));
  if assigned(memoParentDetails) then
  begin
    memoParentDetails.Lines.Clear;
    readLogMetadata(FPath, outLogMetadata);
    WriteRecordToMemo(outLogMetadata, memoParentDetails, FPath);
  end;
end;

procedure TframeRecentTile.imgOverlayMouseLeave(Sender: TObject);
var
  memoParentDetails : TMemo;
begin
  shapeBG.Brush.Color := ColorLeave;
  lblFilename.Font.Style := [];

  // Since the frame is created at runtime, it can't directly access the
  // parent (formStartDialog) or its components. So we have to search for
  // memoDetails to apply properties.
  memoParentDetails := TMemo(Owner.FindComponent('memoDetails'));
  if assigned(memoParentDetails) then
  begin
    memoParentDetails.Lines.Clear;
  end;
end;

procedure TframeRecentTile.imgOverlayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  shapeBG.Brush.Color := ColorUp;
  lblFilename.Font.Style := [fsItalic];
end;

constructor TframeRecentTile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := '';
end;

procedure TframeRecentTile.setColorEnter(newColor: TColor);
begin
  ColorEnter := newColor;
end;

procedure TframeRecentTile.setColorLeave(newColor: TColor);
begin
  ColorLeave := newColor;
end;

procedure TframeRecentTile.setColorInitial(newColor: TColor);
begin
  shapeBG.Brush.Color := newColor;
end;

procedure TframeRecentTile.setColorDown(newColor: TColor);
begin
  ColorDown := newColor;
end;

procedure TframeRecentTile.setColorUp(newColor: TColor);
begin
  ColorUp := newColor;
end;

procedure TframeRecentTile.setFilename(newFilename: String);
begin
  lblFilename.Caption := newFilename;
end;

procedure TframeRecentTile.setPath(newPath: String);
begin
  FPath := newPath;
end;


end.

