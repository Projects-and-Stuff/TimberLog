unit formUnitLogbook;

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
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, SynHighlighterAny,
  SynExportHTML, ZVDateTimePicker, Forms, Controls, Graphics, Dialogs,
  IniPropStorage, StdCtrls, Menus, ExtCtrls, unitClassLogbook,
  unitRecordLogMetadata, LCLIntf, ComCtrls, unitStartFunctions,
  unitDefinitions, dmUnitDBTools, PASVirtualDBScrollRichMemo;

type

  { TformLogbook }

  TformLogbook = class(TForm)
    Button1: TButton;
    chkLateEntry: TCheckBox;
    chkFilterByDate: TCheckBox;
    cmbCategory: TComboBox;
    dtEnd: TZVDateTimePicker;
    dtStart: TZVDateTimePicker;
    FontDialog1: TFontDialog;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblCategory: TLabel;
    lblAddNewEntry: TLabel;
    lblReset: TLabel;
    lblSearch: TLabel;
    LabeledEdit1: TLabeledEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mnuUpdate: TMenuItem;
    mnuViewManual: TMenuItem;
    mnuVisitWebsite: TMenuItem;
    mnuAbout: TMenuItem;
    mnuAudit: TMenuItem;
    MenuItem14: TMenuItem;
    mnuViewDetails: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    mnuFontPlus: TMenuItem;
    mnuOpen: TMenuItem;
    mnuChangeFont: TMenuItem;
    mnuFontMinus: TMenuItem;
    mnuPrintCurrView: TMenuItem;
    mnuPrintEntireLog: TMenuItem;
    mnuExportCurrViewText: TMenuItem;
    mnuExportEntireLogText: TMenuItem;
    mnuExportCurrViewPDF: TMenuItem;
    mnuExportEntireLogPDF: TMenuItem;
    mnuNewLogbook: TMenuItem;
    mnuExit: TMenuItem;
    MenuItem5: TMenuItem;
    mnuClose: TMenuItem;
    mnuThisLogbook: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    LogView: TPASVirtualDBScrollRichMemo;
    Panel2: TPanel;
    Panel3: TPanel;
    Shape1: TShape;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    SynAnySyn1: TSynAnySyn;
    SynAutoComplete1: TSynAutoComplete;
    LogEdit: TSynEdit;
    SynExporterHTML1: TSynExporterHTML;
    txtSearch: TLabeledEdit;
    dtLateEntry: TZVDateTimePicker;
    procedure chkFilterByDateChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblAddNewEntryClick(Sender: TObject);
    procedure lblResetClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuChangeFontClick(Sender: TObject);
    procedure mnuUpdateClick(Sender: TObject);
    procedure mnuViewManualClick(Sender: TObject);
    procedure mnuVisitWebsiteClick(Sender: TObject);
    procedure mnuFontPlusClick(Sender: TObject);
    procedure mnuFontMinusClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuNewLogbookClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuCloseClick(Sender: TObject);
    procedure LabelAsButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LabelAsButtonMouseLeave(Sender: TObject);
    procedure LabelAsButtonMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelAsButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure txtSearchKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    constructor Create(AOwner: TComponent; const inputLogbook : TLogbook);
    constructor Create(AOwner: TComponent; const filepath: String);
    { public declarations }
  end;

var
  formLogbook: TformLogbook;
  ATLogbook : TLogbook;

implementation

{$R *.lfm}

uses
  formUnitStartDialog;

{ TformLogbook }

// Used to open an existing logbook
constructor TformLogbook.Create(AOwner: TComponent; const filepath : String);
begin
  inherited Create(AOwner);

  ATLogbook.Path := filepath;
  ATLogbook.openLogbook;

end;

// Used when a new logbook has been created by formStartDialog
constructor TformLogbook.Create(AOwner: TComponent; const inputLogbook : TLogbook);
begin
  {$ifdef dbgTimberLog} DebugLnEnter(ClassName, '.Create INIT'); {$endif}
  inherited Create(AOwner);
  ATLogbook.Create;
  ATLogbook := inputLogbook;

  ATLogbook.headerMark := RecHeader;
  ATLogbook.footerMark := RecFooter;

  // If there are no logbook errors set, make the "This Logbook" menu item enabled
  if ATLogbook.isError = '' then
  begin
    mnuThisLogbook.Enabled := True;
  end;

  {$ifdef dbgTimberLog} DebugLnExit(ClassName, '.Create DONE'); {$endif}

end;

procedure TformLogbook.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.FormClose'); {$endif}
  IniPropStorage1.Save; // Save SessionProperties

  formStartDialog.Visible := True; // Show formStartDialog before we close this form

  ShowMessage(DateTimeToStr(FileDateToDateTime(ATLogbook.DTOpened)));

  try
    dmDBTools.closeAll; // Make sure the database is closed before we try to perform file operations
    ATLogbook.CloseLogbook(); // Finalize the logbook and add LogMetadata to the end
  finally
    if ATLogbook.isError <> '' then
    begin
      ShowMessage(ATLogbook.isError); // Show any errors
    end;
  end;

  ShowMessage(DateTimeToStr(FileDateToDateTime(ATLogbook.LogMetadata.DTOpened)));

end;

procedure TformLogbook.FormCreate(Sender: TObject);
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.FormCreate'); {$endif}
  Splitter1.Color := clbMedium;
  Splitter2.Color := clbMedium;
  IniPropStorage1.IniFileName := GetAppConfigDir(True) + '\TimberLog.ini';
end;

procedure TformLogbook.FormShow(Sender: TObject);
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.FormShow'); {$endif}
  dtEnd.DateTime := Now;

  lblAddNewEntry.Font.Color := clbVividTextDefault;
  lblReset.Font.Color := clbVividTextDefault;
  lblSearch.Font.Color := clbVividTextDefault;

end;

procedure TformLogbook.lblAddNewEntryClick(Sender: TObject);
begin

  // Verify the length of the entry lines

  // Verify name and entry are present

  // if chkLateEntry is checked, make sure a valid date (in the past) is selected




  ////richmemoLogView.SelStart:=Length(richmemoLogView.Lines.Text)-1; // Scroll to the end (most recent entry)
end;

procedure TformLogbook.lblResetClick(Sender: TObject);
begin
  txtSearch.Clear;
  chkFilterByDate.Checked := False;

  dtStart.Date := Now - 90;
  dtEnd.Date := Now;

  // Clear categories

end;

procedure TformLogbook.mnuAboutClick(Sender: TObject);
begin

end;

procedure TformLogbook.mnuChangeFontClick(Sender: TObject);
begin
  FontDialog1.Font := LogView.ERichMemo.Font;
  if FontDialog1.Execute then
  begin
    LogView.ERichMemo.Font := FontDialog1.Font;
    LogEdit.Font := FontDialog1.Font;
  end;
end;

procedure TformLogbook.mnuUpdateClick(Sender: TObject);
begin

end;

procedure TformLogbook.mnuViewManualClick(Sender: TObject);
begin
  OpenDocument('TimberLogHelp\index.html');
end;

procedure TformLogbook.mnuVisitWebsiteClick(Sender: TObject);
begin
  OpenURL('http://www.projectsandstuff.com/TimberLog/');
end;

procedure TformLogbook.mnuFontPlusClick(Sender: TObject);
begin
  if LogView.ERichMemo.Font.Size < 62 then
  begin
    LogView.ERichMemo.Font.Size := LogView.ERichMemo.Font.Size + 2;
  end;

  if LogEdit.Font.Size < 62 then
  begin
    LogEdit.Font.Size := LogEdit.Font.Size + 2;
  end;
end;

procedure TformLogbook.mnuFontMinusClick(Sender: TObject);
begin
  if LogView.ERichMemo.Font.Size > 7 then
  begin
    LogView.ERichMemo.Font.Size := LogView.ERichMemo.Font.Size - 2;
  end;

  if LogEdit.Font.Size > 7 then
  begin
    LogEdit.Font.Size := LogEdit.Font.Size - 2;
  end;
end;

procedure TformLogbook.mnuOpenClick(Sender: TObject);
begin
  unitStartFunctions.resetPages;

  formStartDialog.pgOpenLogbook.Show;
  formStartDialog.lblBack.Show;
  Close;
end;

procedure TformLogbook.mnuNewLogbookClick(Sender: TObject);
begin
  unitStartFunctions.resetPages;
  formStartDialog.Notebook1.PageIndex := 0;
  Close;
end;

procedure TformLogbook.mnuExitClick(Sender: TObject);
begin
  Close;
  Application.Terminate;
end;

procedure TformLogbook.mnuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TformLogbook.chkFilterByDateChange(Sender: TObject);
begin
  if chkFilterByDate.Checked = True then
  begin
    dtStart.Enabled := True;
    dtEnd.Enabled := True;
  end
  else
  begin
    dtStart.Enabled := False;
    dtEnd.Enabled := False;
  end;

end;


// ToDo: Combine all "LabelAsButton" procedures in formUnitLogbook
// and formUnitStartDialog within unitStartFunctions.
// Also, rename unitStartFunctions to unitHelperFunctions
procedure TformLogbook.LabelAsButtonMouseLeave(Sender: TObject);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbVividTextDefault;
  end;
end;

procedure TformLogbook.LabelAsButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbVividTextClicked;
  end;
end;

procedure TformLogbook.LabelAsButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbVividTextMouseOver;
  end;
end;

procedure TformLogbook.LabelAsButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbVividTextMouseOver;
  end;
end;

procedure TformLogbook.txtSearchKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then
    begin
      //PerformSearch;
    end;
end;

initialization
  ATLogbook := TLogbook.Create;
  {$ifdef dbgTimberLog} DebugLn('TLogbook Created'); {$endif}

end.

