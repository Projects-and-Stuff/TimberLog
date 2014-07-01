unit unitTypeTile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, StrHolder, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, Dialogs, messages, windows, LMessages, unitStartFunctions;

type

  { TframeTypeTile }

  TframeTypeTile = class(TFrame)
    imgOverlay: TImage;
    lblTitle: TLabel;
    lblDescription: TLabel;
    lblPath: TLabel;
    shapeBG: TShape;
    DescriptionStrings: TStrHolder;
    procedure imgOverlayClick(Sender: TObject);
    procedure imgOverlayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgOverlayMouseEnter(Sender: TObject);
    procedure imgOverlayMouseLeave(Sender: TObject);
    procedure imgOverlayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    ColorEnter : TColor;
    ColorLeave : TColor;
    ColorDown : TColor;
    ColorUp : TColor;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure setColorEnter(newColor : TColor);
    procedure setColorLeave(newColor : TColor);
    procedure setColorInitial(newColor : TColor);
    procedure setColorDown(newColor : TColor);
    procedure setColorUp(newColor : TColor);
    procedure setTitle(newTitle : String);
    procedure setDescription(newDescription : String);
    procedure setPath(newPath : String);
    procedure Kill;
  end;

implementation

uses
  unitDefinitions;

{$R *.lfm}

{ TframeTypeTile }

constructor TframeTypeTile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := '';
end;

destructor TframeTypeTile.Destroy;
begin
  inherited Destroy;
end;

procedure TframeTypeTile.imgOverlayMouseEnter(Sender: TObject);
var
  memoParentDetails : TMemo;
begin
  shapeBG.Brush.Color := ColorEnter;

  // Since the frame is created at runtime, it can't directly access the
  // parent (formStartDialog) or its components. So we have to search for
  // memoDetails to apply properties.
  memoParentDetails := TMemo(Owner.FindComponent('memoDetails'));
  if assigned(memoParentDetails) then
  begin
    memoParentDetails.Lines.Clear;
    memoParentDetails.Lines := DescriptionStrings.Strings;
  end;

end;

procedure TframeTypeTile.imgOverlayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  shapeBG.Brush.Color := ColorDown;
end;

procedure TframeTypeTile.imgOverlayClick(Sender: TObject);
var
  lblParentTypepath : TLabel;
  lblParentNewLogbook : TLabel;
  notebookParent : TNotebook;
  memoParentDetails : TMemo;
  listboxParentCategories : TListBox;
  errorMsg, Logbook_Type : String;
  Description, Default_Categories : TStringList;

begin
  shapeBG.Brush.Color := ColorLeave;

  Description := TStringList.Create;
  Default_Categories := TStringList.Create;

  processTypeFile(lblPath.Caption, errorMsg, Logbook_Type, Description, Default_Categories);
  //ShowMessage((Default_Categories[0]));

  // Since the frame is created at runtime, it can't directly access the
  // parent (formStartDialog) or its components. So we have to search for
  // memoDetails etc to apply properties.
  notebookParent := TNotebook(Owner.FindComponent('Notebook1')); // Change pages first, so the memo and listbox aren't cleared with resetPages
  if assigned(notebookParent) then
  begin
    notebookParent.PageIndex := 1;
  end;

  listboxParentCategories := TListBox(Owner.FindComponent('listboxCategories'));
  if assigned(listboxParentCategories) then
  begin
    listboxParentCategories.Items := Default_Categories;
  end;

  lblParentNewLogbook := TLabel(Owner.FindComponent('lblNewLogbook'));
  if assigned(lblParentNewLogbook) then
  begin
    lblParentNewLogbook.Caption := 'New Logbook - Type: ' + lblTitle.Caption;
  end;

  lblParentTypepath := TLabel(Owner.FindComponent('lblTypepath'));
  if assigned(lblParentTypepath) then
  begin
    lblParentTypepath.Caption := lblPath.Caption;
  end;

  memoParentDetails := TMemo(Owner.FindComponent('memoDetails'));
  if assigned(memoParentDetails) then
  begin
    memoParentDetails.Lines := Description;
  end;


  Description.Free;
  Default_Categories.Free;

end;

procedure TframeTypeTile.imgOverlayMouseLeave(Sender: TObject);
var
  memoParentDetails : TMemo;
  notebookParent : TNotebook;
begin
  shapeBG.Brush.Color := ColorLeave;

  // Since the frame is created at runtime, it can't directly access the
  // parent (formStartDialog) or its components. So we have to search for
  // memoDetails to apply properties.

  // Check whether we're on page 0. If so, clear the memo lines when MouseLeave
  notebookParent := TNotebook(Owner.FindComponent('Notebook1'));
  if assigned(notebookParent) then
  begin
    if notebookParent.PageIndex = 0 then
    begin
      memoParentDetails := TMemo(Owner.FindComponent('memoDetails'));
      if assigned(memoParentDetails) then
      begin
        memoParentDetails.Lines.Clear;
      end;
    end;

  end;
end;

procedure TframeTypeTile.imgOverlayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  shapeBG.Brush.Color := ColorUp;
end;

procedure TframeTypeTile.setColorEnter(newColor: TColor);
begin
  ColorEnter := newColor;
end;

procedure TframeTypeTile.setColorLeave(newColor: TColor);
begin
  ColorLeave := newColor;
end;

procedure TframeTypeTile.setColorInitial(newColor: TColor);
begin
  shapeBG.Brush.Color := newColor;
end;

procedure TframeTypeTile.setColorDown(newColor: TColor);
begin
  ColorDown := newColor;
end;

procedure TframeTypeTile.setColorUp(newColor: TColor);
begin
  ColorUp := newColor;
end;

procedure TframeTypeTile.setTitle(newTitle: String);
begin
  lblTitle.Caption := newTitle;
end;

procedure TframeTypeTile.setDescription(newDescription: String);
begin
  if Length(newDescription) > 90 then
  begin
    lblDescription.Caption := Copy(newDescription, 0, 87) + '...';
  end
  else
  begin
    lblDescription.Caption := newDescription;
  end;

end;

procedure TframeTypeTile.setPath(newPath: String);
begin
  lblPath.Caption := newPath;
end;

procedure TframeTypeTile.Kill;
begin
  Application.ReleaseComponent(Self);
end;


end.

