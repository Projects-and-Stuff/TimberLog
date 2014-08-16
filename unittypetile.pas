unit unitTypeTile;

/////////////////////////////////////////////////////////////////////////////
//                   This file is part of TimberLog.                       //
/////////////////////////////////////////////////////////////////////////////
//  TimberLog is free software: you can redistribute it and/or modify      //
//  it under the terms of the GNU General Public License as published by   //
//  the Free Software Foundation, either version 3 of the License, or      //
//  (at your option) any later version.                                    //
//                                                                         //
//  TimberLog is distributed in the hope that it will be useful,           //
//  but WITHOUT ANY WARRANTY; without even the implied warranty of         //
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          //
//  GNU General Public License for more details.                           //
//                                                                         //
//  You should have received a copy of the GNU General Public License      //
//  along with TimberLog.  If not, see <http://www.gnu.org/licenses/>.     //
/////////////////////////////////////////////////////////////////////////////
//                     Copyright 2014 Jack D Linke                         //
/////////////////////////////////////////////////////////////////////////////

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
    procedure SetColorEnter(NewColor : TColor);
    procedure SetColorLeave(NewColor : TColor);
    procedure SetColorInitial(NewColor : TColor);
    procedure SetColorDown(NewColor : TColor);
    procedure SetColorUp(NewColor : TColor);
    procedure SetTitle(NewTitle : String);
    procedure SetDescription(NewDescription : String);
    procedure SetPath(NewPath : String);
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

procedure TframeTypeTile.SetColorEnter(NewColor: TColor);
begin
  ColorEnter := NewColor;
end;

procedure TframeTypeTile.SetColorLeave(NewColor: TColor);
begin
  ColorLeave := NewColor;
end;

procedure TframeTypeTile.SetColorInitial(NewColor: TColor);
begin
  shapeBG.Brush.Color := NewColor;
end;

procedure TframeTypeTile.SetColorDown(NewColor: TColor);
begin
  ColorDown := NewColor;
end;

procedure TframeTypeTile.SetColorUp(NewColor: TColor);
begin
  ColorUp := NewColor;
end;

procedure TframeTypeTile.SetTitle(NewTitle: String);
begin
  lblTitle.Caption := NewTitle;
end;

procedure TframeTypeTile.SetDescription(NewDescription: String);
begin
  if Length(NewDescription) > 90 then
  begin
    lblDescription.Caption := Copy(NewDescription, 0, 87) + '...';
  end
  else
  begin
    lblDescription.Caption := NewDescription;
  end;

end;

procedure TframeTypeTile.SetPath(NewPath: String);
begin
  lblPath.Caption := NewPath;
end;

procedure TframeTypeTile.Kill;
begin
  Application.ReleaseComponent(Self);
end;


end.

