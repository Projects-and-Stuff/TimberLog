unit formUnitLogbook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, SynHighlighterAny,
  SynExportHTML, RichMemo, ZVDateTimePicker, Forms, Controls,
  Graphics, Dialogs, IniPropStorage, StdCtrls, Menus, ExtCtrls,
  unitClassLogbook, unitRecordLogMetadataExt, unitRecordLogMetadata, LCLIntf,
  ComCtrls, unitStartFunctions, unitDefinitions;

type

  { TformLogbook }

  TformLogbook = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    chkFilterByDate: TCheckBox;
    dtEnd: TZVDateTimePicker;
    dtStart: TZVDateTimePicker;
    FontDialog1: TFontDialog;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblAddNewEntry: TLabel;
    lblReset: TLabel;
    lblSearch: TLabel;
    LabeledEdit1: TLabeledEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
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
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    richmemoLogView: TRichMemo;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    SynAnySyn1: TSynAnySyn;
    SynCompletion1: TSynCompletion;
    syneditLogEdit: TSynEdit;
    SynExporterHTML1: TSynExporterHTML;
    txtSearch: TLabeledEdit;
    ZVDateTimePicker1: TZVDateTimePicker;
    procedure btnResetSearchClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblAddNewEntryClick(Sender: TObject);
    procedure mnuChangeFontClick(Sender: TObject);
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
    constructor Create(AOwner: TComponent; const filepath: String; const typepath : String;
      const InputMetadataExt: RLogMetadataExt);
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

// Used to open a logbook
constructor TformLogbook.Create(AOwner: TComponent; const filepath : String);
begin
  inherited Create(AOwner);

  ATLogbook.OpenLogbook(filepath);
end;

// Used to create a new logbook
constructor TformLogbook.Create(AOwner: TComponent; const filepath : String; const typepath : String; const InputMetadataExt : RLogMetadataExt);
begin
  inherited Create(AOwner);

  ShowMessage(IntToStr(Length(InputMetadataExt.Categories)))

  //ATLogbook.NewLogbook(filepath, InputMetadataExt);
end;


procedure TformLogbook.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IniPropStorage1.Save; // Save SessionProperties

  formStartDialog.Visible := True; // Show formStartDialog before we close this form

  ATLogbook.CloseLogbook();
end;

procedure TformLogbook.FormCreate(Sender: TObject);
begin
  Splitter1.Color := clbMedium;
  Splitter2.Color := clbMedium;
end;

procedure TformLogbook.FormShow(Sender: TObject);
begin
  dtEnd.DateTime := Now;

  lblAddNewEntry.Font.Color := clbVividTextDefault;
  lblReset.Font.Color := clbVividTextDefault;
  lblSearch.Font.Color := clbVividTextDefault;

end;

procedure TformLogbook.lblAddNewEntryClick(Sender: TObject);
begin






  richmemoLogView.SelStart:=Length(richmemoLogView.Lines.Text)-1; // Scroll to the end (most recent entry)
end;

procedure TformLogbook.mnuChangeFontClick(Sender: TObject);
begin
  FontDialog1.Font := richmemoLogView.Font;
  if FontDialog1.Execute then
  begin
    richmemoLogView.Font := FontDialog1.Font;
    syneditLogEdit.Font := FontDialog1.Font;
  end;
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
  if richmemoLogView.Font.Size < 62 then
  begin
    richmemoLogView.Font.Size := richmemoLogView.Font.Size + 2;
  end;

  if syneditLogEdit.Font.Size < 62 then
  begin
    syneditLogEdit.Font.Size := syneditLogEdit.Font.Size + 2;
  end;
end;

procedure TformLogbook.mnuFontMinusClick(Sender: TObject);
begin
  if richmemoLogView.Font.Size > 7 then
  begin
    richmemoLogView.Font.Size := richmemoLogView.Font.Size - 2;
  end;

  if syneditLogEdit.Font.Size > 7 then
  begin
    syneditLogEdit.Font.Size := syneditLogEdit.Font.Size - 2;
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

procedure TformLogbook.btnResetSearchClick(Sender: TObject);
begin

end;

procedure TformLogbook.btnSearchClick(Sender: TObject);
begin

end;

procedure TformLogbook.Button1Click(Sender: TObject);
var
  testLogMetadata : RLogMetadata;
begin

  testLogMetadata.headerMark := '>TimberLog<';
  testLogMetadata.footerMark := '>TimberLog<';
  testLogMetadata.DTAccessed := DateTimeToFileDate(Now);
  testLogMetadata.DTOpened := DateTimeToFileDate(Now);
  testLogMetadata.checksum := '4444444444444444444444444444444444444444444444444444';
  testLogMetadata.OpenedBy := 'Johnny';
  testLogMetadata.logName := 'Serial Number';
  testLogMetadata.logDescription := 'Yo';



  ATLogbook.LogMetadata := testLogMetadata;

  ShowMessage(ATLogbook.LogMetadata.footerMark + ATLogbook.LogMetadata.checksum);


  //ATLogbook.NewLogbook();
end;

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

end.

