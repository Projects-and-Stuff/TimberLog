unit formUnitStartDialog;

// GUI-related code for formStartDialog

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  types, LCLType, ExtCtrls, Buttons, ShellCtrls, LCLIntf, IniPropStorage,
  formUnitLogbook, unitDefinitions, unitStartFunctions, unitRecordLogMetadata,
  ComCtrls, unitRecordLogMetadataExt, dmUnitCrypt;

type

  { TformStartDialog }

  TformStartDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    btnAddCategory: TButton;
    btnDeleteCategory: TButton;
    chkAllowCategories: TCheckBox;
    chkAllowLateEntries: TCheckBox;
    chkAllowAddCategories: TCheckBox;
    chkPassPerUser: TCheckBox;
    chkPassMaster: TCheckBox;
    chkPassToExport: TCheckBox;
    chkPassToPrint: TCheckBox;
    cmbTFormat: TComboBox;
    cmbType: TComboBox;
    cmbDFormat: TComboBox;
    txtPassToExport: TEdit;
    txtPassToPrint: TEdit;
    Label20: TLabel;
    SaveDialog1: TSaveDialog;
    txtCategory: TEdit;
    GroupBox1: TGroupBox;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    lblNow: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblOpenOtherLogbooks: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    lblBack: TLabel;
    listboxCategories: TListBox;
    listboxRecent: TListBox;
    memoLogDescription: TMemo;
    memoDetails: TMemo;
    Notebook1: TNotebook;
    pgOpenLogbook: TPage;
    pgNewTypes: TPage;
    pgNewLogbook: TPage;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    ShellListView1: TShellListView;
    ShellTreeView1: TShellTreeView;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    txtOpenedBy: TEdit;
    txtMasterPass: TEdit;
    txtLogName: TEdit;
    procedure btnAddCategoryClick(Sender: TObject);
    procedure btnDeleteCategoryClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure chkAllowCategoriesChange(Sender: TObject);
    procedure cmbDFormatChange(Sender: TObject);
    procedure cmbTFormatChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label13Click(Sender: TObject);
    procedure lblBackClick(Sender: TObject);
    procedure lblOpenOtherLogbooksClick(Sender: TObject);
    procedure LabelAsButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LabelAsButtonMouseLeave(Sender: TObject);
    procedure LabelAsButtonMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelAsButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure listboxRecentDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure listboxRecentMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure listboxRecentMouseEnter(Sender: TObject);
    procedure listboxRecentMouseLeave(Sender: TObject);
    procedure listboxRecentMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure listboxRecentMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShellListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ShellListView1DblClick(Sender: TObject);
    procedure ShellListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure WriteRecordToMemo(LogMetadata : RLogMetadata);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  formStartDialog: TformStartDialog;

implementation

{$R *.lfm}

{ TformStartDialog }

procedure TformStartDialog.listboxRecentDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox).Canvas do begin
    Brush.Style := bsSolid;
    if not listboxRecent.Selected[Index] then begin
       //Brush.Color := clbMedium;
       Font.Color:=clSkyBlue;
    end;
    FillRect(aRect);
    Brush.Style := bsSolid;
    TextOut(aRect.Left, aRect.Top, (Control as TListBox).Items[Index]);
  end;

  if odNoFocusRect in State then
  begin
    listboxRecent.Canvas.Brush.Color := clWhite;
    listboxRecent.Canvas.FillRect(ARect);
    listboxRecent.Canvas.Font.Color := clbLightest;
    listboxRecent.Canvas.TextOut(ARect.Left, ARect.Top, listboxRecent.Items[Index]);
  end;

  if odSelected in State then
  begin
    listboxRecent.Canvas.Brush.Color := clWhite;
    listboxRecent.Canvas.FillRect(ARect);
    listboxRecent.Canvas.Font.Color:=clbMedium;
    listboxRecent.Canvas.TextOut(ARect.Left, ARect.Top, listboxRecent.Items[Index]);
    listboxRecent.Canvas.Pen.Color := clWhite; // Make the selecton rectangle invisible
    listboxRecent.Canvas.DrawFocusRect(aRect)
  end;

end;

procedure TformStartDialog.LabelAsButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbMedium;
  end;
end;

procedure TformStartDialog.LabelAsButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbMedium;
  end;
end;

procedure TformStartDialog.LabelAsButtonMouseLeave(Sender: TObject);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbMedium;
  end;
end;

procedure TformStartDialog.LabelAsButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clBlack;
  end;
end;

procedure TformStartDialog.lblOpenOtherLogbooksClick(Sender: TObject);
begin

  try
    if not DirectoryExists(returnDocumentsPath + '\TimberLog\') then  // Check if TimberLog user directory exists
    begin
      MkDir(returnDocumentsPath + '\TimberLog\');
    end;

    ShellTreeView1.Path := returnDocumentsPath + '\TimberLog\';
  finally

  end;

  Notebook1.PageIndex := 2;
  lblBack.Visible := True;
end;

procedure TformStartDialog.FormShow(Sender: TObject);
begin
  Splitter1.Color := clbMedium;
  Splitter2.Color := clbMedium;
  Shape1.Pen.Color := clbMedium;
  lblOpenOtherLogbooks.Font.Color := clbMedium;
  listboxRecent.Font.Color := clbMedium;
  //listboxRecent.Canvas.Font.Color := clbMedium;
  ShellListView1.Font.Color := clbMedium;
  pgNewLogbook.Color:=clbLightest;



  ShellListView1.Mask := '*.logb'; // Only display *.logb files
  Notebook1.PageIndex := 0;

end;

procedure TformStartDialog.Label13Click(Sender: TObject);
var
  LogMetadataExt : RLogMetadataExt;
  newFormLogbook : TformLogbook;
  filepath : String;
  i : Integer;
begin



  // Verify required fields are filled in !!!!!!!!!!!!!!!




  // Set LogMetadataExt to the values in the form
  LogMetadataExt.logName := txtLogName.Text;
  LogMetadataExt.logDescription := memoLogDescription.Text;
  LogMetadataExt.OpenedBy := txtOpenedBy.Text;
  LogMetadataExt.DTOpened := DateTimeToFileDate(Now);
  LogMetadataExt.DTAccessed := DateTimeToFileDate(Now);
  LogMetadataExt.PassMaster := chkPassMaster.Checked;
  LogMetadataExt.PassPerUser := chkPassPerUser.Checked;
  LogMetadataExt.PassToExport := chkPassToExport.Checked;
  LogMetadataExt.PassToPrint := chkPassToPrint.Checked;
  LogMetadataExt.PassMasterHash:= dmCrypt.hash(txtMasterPass.Text);   // Hashed password
  LogMetadataExt.PassExportHash:= dmCrypt.hash(txtPassToExport.Text); // Hashed password
  LogMetadataExt.PassPrintHash:= dmCrypt.hash(txtPassToPrint.Text);   // Hashed password
  LogMetadataExt.AllowCategories := chkAllowLateEntries.Checked;
  LogMetadataExt.AllowAddCategories := chkAllowAddCategories.Checked;
  LogMetadataExt.AllowLateEntries := chkAllowLateEntries.Checked;
  LogMetadataExt.DTDisplayFormat:=Trim(cmbDFormat.Caption + ' ' + cmbTFormat.Caption);


  for i := 0 to listboxCategories.Count-1 do
  begin
    LogMetadataExt.Categories.Add(listboxCategories.Items.Strings[i]);
  end;



  // Pass the record over to formLogbook

  if SaveDialog1.Execute then
  begin
    //formStartDialog.Visible := False;             // Hide the calling form first
    filepath := ChangeFileExt(SaveDialog1.FileName, '.logb');
    ShowMessage(filepath);
  end;
  //newFormLogbook := TformLogbook.Create(Nil, 'filepath', RLogMetadata); // Use this format when making a new logbook

  //newFormLogbook.ShowModal;                     //Form2 is displayed
  //FreeAndNil(newFormLogbook);                   //Free Form2





end;

procedure TformStartDialog.lblBackClick(Sender: TObject);
begin

  pgNewTypes.Show;
  Notebook1.PageIndex := 0;
  lblBack.Visible := False;
end;

procedure TformStartDialog.Button1Click(Sender: TObject);
var
  newFormLogbook : TformLogbook;
begin
  formStartDialog.Visible := False;             // Hide the calling form first
  newFormLogbook := TformLogbook.Create(Nil, 'filepath'); // This format for opening a file
  //newFormLogbook := TformLogbook.Create(Nil, 'filepath', RLogMetadata); // Use this format when making a new logbook

  newFormLogbook.ShowModal;                     //Form2 is displayed
  FreeAndNil(newFormLogbook);                   //Free Form2
end;

procedure TformStartDialog.btnAddCategoryClick(Sender: TObject);
var
  i : Integer;
  exists : Boolean;
begin

  // If the value of txtCategory.Text isn't already in listboxCategories
  // then add it to the list
  exists:=False;

  if not (Trim(txtCategory.Text) = '') then
  begin

    for i := listboxCategories.Count-1 downto 0 do
    begin
      if (Trim(txtCategory.Text) = listboxCategories.Items[i]) then
      begin
        exists := True;
      end;
    end;

  end
  else
  begin
    exists := True;
  end;

  if exists = False then
  begin
    listboxCategories.Items.Add(Trim(txtCategory.Text));
    txtCategory.Clear;
  end;

  listboxCategories.Sorted := True;
end;

procedure TformStartDialog.btnDeleteCategoryClick(Sender: TObject);
begin
  listboxCategories.Items.Delete(listboxCategories.ItemIndex);
end;

procedure TformStartDialog.Button2Click(Sender: TObject);
begin
  Notebook1.PageIndex:=1;
end;

procedure TformStartDialog.chkAllowCategoriesChange(Sender: TObject);
begin
  if chkAllowCategories.Checked then
  begin
    txtCategory.Enabled := True;
    btnAddCategory.Enabled := True;
    btnDeleteCategory.Enabled := True;
    listboxCategories.Enabled := True;
  end
  else
  begin
    txtCategory.Enabled := False;
    btnAddCategory.Enabled := False;
    btnDeleteCategory.Enabled := False;
    listboxCategories.Enabled := False;
  end;
end;

procedure TformStartDialog.cmbDFormatChange(Sender: TObject);
begin
  lblNow.Caption := FormatDateTime(cmbDFormat.Caption + ' ' + cmbTFormat.Caption, Now);
end;

procedure TformStartDialog.cmbTFormatChange(Sender: TObject);
begin
  lblNow.Caption := FormatDateTime(cmbDFormat.Caption + ' ' + cmbTFormat.Caption, Now);
end;

procedure TformStartDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IniPropStorage1.Save;
end;

procedure TformStartDialog.FormCreate(Sender: TObject);
begin


end;

procedure TformStartDialog.listboxRecentMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ARect : TRect;
begin
  ARect := listboxRecent.ItemRect(listboxRecent.ItemAtPos(Point(X,Y), True));
  listboxRecent.Canvas.Brush.Color := clWhite;
  listboxRecent.Canvas.FillRect(ARect);
  listboxRecent.Canvas.Font.Color:=clBlack;
  listboxRecent.Canvas.TextOut(ARect.Left, ARect.Top, listboxRecent.Items[listboxRecent.ItemAtPos(Point(X,Y), True)]);
end;

procedure TformStartDialog.listboxRecentMouseEnter(Sender: TObject);
var
  i : Integer;
begin
  listboxRecent.SetFocus;
  for i := 0 to listboxRecent.Count-1 do
  begin
    listboxRecentDrawItem(listboxRecent, i, listboxRecent.ItemRect(i), [odNoFocusRect]);
    listboxRecent.Selected[i] := False;
  end;
end;

procedure TformStartDialog.listboxRecentMouseLeave(Sender: TObject);
var
  i :Integer;
begin
  listboxRecent.ItemIndex := -1;

  for i := 0 to listboxRecent.Count-1 do
  begin
    listboxRecentDrawItem(listboxRecent, i, listboxRecent.ItemRect(i), [odNoFocusRect]);
    listboxRecent.Selected[i] := False;
  end;
end;

procedure TformStartDialog.listboxRecentMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  listboxRecent.ItemIndex := listboxRecent.ItemAtPos(Point(X,Y), True);
end;

procedure TformStartDialog.listboxRecentMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ARect : TRect;
begin
  ARect := listboxRecent.ItemRect(listboxRecent.ItemAtPos(Point(X,Y), True));
  listboxRecent.Canvas.Brush.Color := clWhite;
  listboxRecent.Canvas.FillRect(ARect);
  listboxRecent.Canvas.Font.Color:=clbMedium;
  listboxRecent.Canvas.TextOut(ARect.Left, ARect.Top, listboxRecent.Items[listboxRecent.ItemAtPos(Point(X,Y), True)]);
end;

procedure TformStartDialog.ShellListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin

end;

// Create a new TformLogbook and pass the filepath to it
procedure TformStartDialog.ShellListView1DblClick(Sender: TObject);
var
  newFormLogbook : TformLogbook;
begin
  formStartDialog.Visible := False;             // Hide the calling form first
  newFormLogbook := TformLogbook.Create(Nil, ShellListView1.GetPathFromItem(ShellListView1.Selected));

  newFormLogbook.ShowModal;                     //Form2 is displayed
  FreeAndNil(newFormLogbook);                   //Free Form2
end;

procedure TformStartDialog.ShellListView1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  LogMetadata : RLogMetadata;
begin
  LogMetadata := readLogMetadata(ShellListView1.GetPathFromItem(Item));


  WriteRecordToMemo(LogMetadata); // Writes the record contents to the memo
end;

procedure TformStartDialog.SpeedButton1Click(Sender: TObject);
begin
  formStartDialog.Close;
end;

procedure TformStartDialog.SpeedButton2Click(Sender: TObject);
begin
  OpenDocument('TimberLogHelp\index.html');
end;

procedure TformStartDialog.WriteRecordToMemo(LogMetadata : RLogMetadata);
begin
  memoDetails.Lines.Clear;
  memoDetails.Lines.Add('Footer: ' + Trim(LogMetadata.footerMark));
  memoDetails.Lines.Add('Logbook Opened By: ' + Trim(LogMetadata.OpenedBy));
  memoDetails.Lines.Add('Logbook Serial Number/Name: ' + Trim(LogMetadata.logName));
  try
    memoDetails.Lines.Add('Logbook Opened On: ' + DateTimeToStr(FileDateToDateTime(LogMetadata.DTOpened)));
    memoDetails.Lines.Add('Logbook Last Accessed: ' + DateTimeToStr(FileDateToDateTime(LogMetadata.DTAccessed)));
  except
    memoDetails.Lines.Add('Unable to Read Logbook Dates. Likely Corruption');
  end;
  memoDetails.Lines.Add('Logbook Description: ' + Trim(LogMetadata.logDescription));
end;

end.

