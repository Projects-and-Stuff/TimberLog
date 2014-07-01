unit formUnitStartDialog;

// GUI-related code for formStartDialog

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, types, LCLType, ExtCtrls, Buttons, ShellCtrls, LCLIntf,
  IniPropStorage, formUnitLogbook, unitDefinitions, unitStartFunctions,
  unitRecordLogMetadata, ComCtrls, Grids, unitRecordLogMetadataExt, dmUnitCrypt,
  unitTypeTile, unitRecentTile, mrumanager, strutils, Contnrs;

type

  { TformStartDialog }

  TformStartDialog = class(TForm)
    btnAddCategory: TButton;
    btnDeleteCategory: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    chkAllowAddCategories: TCheckBox;
    chkAllowCategories: TCheckBox;
    chkAllowLateEntries: TCheckBox;
    chkPassMaster: TCheckBox;
    chkPassPerUser: TCheckBox;
    chkPassToExport: TCheckBox;
    chkPassToPrint: TCheckBox;
    cmbDFormat: TComboBox;
    cmbTFormat: TComboBox;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label6: TLabel;
    lblTypepath: TLabel;
    Label9: TLabel;
    lblNow: TLabel;
    listboxCategories: TListBox;
    memoLogDescription: TMemo;
    mruMgr: TMRUMenuManager;
    Notebook2: TNotebook;
    pgOptionalSettings: TPage;
    pgBasicSettings: TPage;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    Shape6: TShape;
    TabControl1: TTabControl;
    txtCategory: TEdit;
    txtLogName: TEdit;
    SaveDialog1: TSaveDialog;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblOpenOtherLogbooks: TLabel;
    Label5: TLabel;
    lblNewLogbook: TLabel;
    lblBack: TLabel;
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
    txtMasterPass: TEdit;
    txtMasterPass1: TEdit;
    txtOpenedBy: TEdit;
    txtPassToExport: TEdit;
    txtPassToExport1: TEdit;
    txtPassToPrint: TEdit;
    txtPassToPrint1: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure LabelAsButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LabelAsButtonMouseLeave(Sender: TObject);
    procedure LabelAsButtonMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelAsButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnAddCategoryClick(Sender: TObject);
    procedure btnDeleteCategoryClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure chkAllowCategoriesChange(Sender: TObject);
    procedure chkPassMasterChange(Sender: TObject);
    procedure chkPassToExportChange(Sender: TObject);
    procedure chkPassToPrintChange(Sender: TObject);
    procedure cmbDFormatChange(Sender: TObject);
    procedure cmbTFormatChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label13Click(Sender: TObject);
    procedure lblBackClick(Sender: TObject);
    procedure lblOpenOtherLogbooksClick(Sender: TObject);
    procedure pgNewLogbookBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure pgNewTypesBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure pgOpenLogbookBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure ShellListView1DblClick(Sender: TObject);
    procedure ShellListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure WriteRecordToMemo(LogMetadata : RLogMetadata);
  private
    { private declarations }
  public
    { public declarations }
    FrameTypeTile : array of TframeTypeTile;
    FrameRecentTile : array of TframeRecentTile;
  end;

var
  formStartDialog: TformStartDialog;
  typeTileList : TObjectList;
  recentTileList : TObjectList;

implementation

{$R *.lfm}


{ TformStartDialog }

procedure TformStartDialog.LabelAsButtonMouseLeave(Sender: TObject);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbVividTextDefault;
  end;
end;

procedure TformStartDialog.LabelAsButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbVividTextClicked;
  end;
end;

procedure TformStartDialog.FormDestroy(Sender: TObject);
begin

end;

procedure TformStartDialog.LabelAsButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbVividTextMouseOver;
  end;
end;

procedure TformStartDialog.LabelAsButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with (Sender as TLabel) do begin
    (Sender as TLabel).Font.Color := clbVividTextMouseOver;
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
var
  i, tileLength : Integer;
  fileList : TStringList;
  errorMsg, Logbook_Type : String;
  Description : TStringList;
begin

  typeTileList := TObjectList.create();
  recentTileList := TObjectList.create();

  mruMgr.IniFileName := GetAppConfigDir(True) + '\Test.ini';
  mruMgr.IniSection := 'RecentLogbooks';
  mruMgr.MaxRecent := 9;
  mruMgr.ShowRecentFiles;


  if ScrollBox1.ControlCount > 0 then
  begin
    for i := ScrollBox1.ControlCount-1 downto 0 do
    begin
      ScrollBox1.Controls[i].Destroy;
    end;
  end;

  if ScrollBox2.ControlCount > 0 then
  begin
    for i := ScrollBox2.ControlCount-1 downto 0 do
    begin
      ScrollBox2.Controls[i].Destroy;
    end;
  end;




  //SetLength(FrameRecentTile, 0);
  SetLength(FrameRecentTile, 1); // Required, or we'll get Seg faults

  if mruMgr.Recent.Count > 0 then
  begin
    for i := mruMgr.Recent.Count-1 downto 0 do
    begin
      SetLength(FrameRecentTile, Length(FrameRecentTile)+1);
      FrameRecentTile[i] := TframeRecentTile.Create(formStartDialog);
      FrameRecentTile[i].Parent := ScrollBox2;
      FrameRecentTile[i].Align := alTop;
      FrameRecentTile[i].setColorInitial(clWhite);
      FrameRecentTile[i].setColorLeave(clWhite);
      FrameRecentTile[i].setColorEnter(clbBGMouseOver);
      FrameRecentTile[i].setColorDown(clbBGClicked);
      FrameRecentTile[i].setColorUp(clbBGMouseOver);
      FrameRecentTile[i].setFilename(ExtractFileNameOnly(mruMgr.Recent.Strings[i]));
    end;
  end
  else
  begin
    ShowMessage('No recent files');
  end;



  fileList := TStringList.Create;
  getTypeFileList(fileList);
  fileList.Sort;

  SetLength(FrameTypeTile, 0);


  ////////////////// CHECK THE ERROR MESSAGE BEFORE PROCEEDING! ////////////
  if fileList.Strings[0] = 'None' then
  begin
    memoDetails.Lines.Clear;
    memoDetails.Lines.AddText('No Logbook types were found.');
    memoDetails.Lines.AddText(' ');
    memoDetails.Lines.AddText('Please refer to the manual in order to correct this problem and add new types.');
  end
  else
  begin
    SetLength(FrameTypeTile, fileList.Count);
    for i := fileList.Count-1 downto 0 do
    begin

      Description := TStringList.Create;

      processTypeFile(fileList[i], errorMsg, Logbook_Type, Description);

      FrameTypeTile[i] := TframeTypeTile.Create(formStartDialog);


      FrameTypeTile[i].Parent := ScrollBox1;

      FrameTypeTile[i].Align := alTop;
      FrameTypeTile[i].setColorInitial(clbBGDefault);
      FrameTypeTile[i].setColorLeave(clbBGDefault);
      FrameTypeTile[i].setColorEnter(clbBGMouseOver);
      FrameTypeTile[i].setColorDown(clbBGClicked);
      FrameTypeTile[i].setColorUp(clbBGMouseOver);
      FrameTypeTile[i].setTitle(Logbook_Type);
      FrameTypeTile[i].setDescription(Description.Strings[0]);
      FrameTypeTile[i].setPath(fileList.Strings[i]);

      FrameTypeTile[i].DescriptionStrings.Strings := Description;
      Description.Free;

      //ShowMessage(IntToStr(fileList.Count) + ', ' + IntToStr(Length(FrameTypeTile)) + ', ' + Logbook_Type);

    end;
  end;

  ShowMessage(IntToStr(Length(FrameTypeTile)));

  fileList.Free;
  Splitter1.Color := clbMedium;
  Splitter2.Color := clbMedium;
  Shape1.Pen.Color := clbMedium;
  lblOpenOtherLogbooks.Font.Color := clbVividTextDefault;
  lblBack.Font.Color := clbVividTextDefault;
  ShellListView1.Font.Color := clbVividTextDefault;
  pgNewLogbook.Color := clbBGDefault;


  ShellListView1.Mask := '*.logb'; // Only display *.logb files
  Notebook1.PageIndex := 0;

end;

procedure TformStartDialog.Label13Click(Sender: TObject);
var
  LogMetadataExt : RLogMetadataExt;
  newFormLogbook : TformLogbook;
  filepath : String;
  i : Integer;
  hasError : Boolean;
begin



  // Verify required fields are filled in !!!!!!!!!!!!!!!
  hasError := False;

  if not (txtMasterPass.Text = txtMasterPass1.Text) then
  begin
    hasError := True;
    ShowMessage('The text in both "Master Password" textboxes must match');
  end;

  if not (txtPassToExport.Text = txtPassToExport1.Text) then
  begin
    hasError := True;
    ShowMessage('The text in both "Password Required to Export" textboxes must match');
  end;

  if not (txtPassToPrint.Text = txtPassToPrint1.Text) then
  begin
    hasError := True;
    ShowMessage('The text in both "Password Required to Print" textboxes must match');
  end;

  if (Trim(txtLogName.Text) = '') then
  begin
    hasError := True;
    ShowMessage('Please enter a name for this logbook');
  end;

  if (Trim(txtOpenedBy.Text) = '') then
  begin
    hasError := True;
    ShowMessage('Please enter the name of the person opening this logbook');
  end;

  if (Trim(memoLogDescription.Text) = '') then
  begin
    hasError := True;
    ShowMessage('Please enter a description for this logbook');
  end;


  if hasError = False then
  begin
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

    // Convert the contents of the Categories listbox to a delimited string
    for i := 0 to listboxCategories.Count-1 do
    begin
      ReplaceText(listboxCategories.Items.Strings[i], '|', ':');
      if not i = listboxCategories.Count-1 then
      begin
        LogMetadataExt.Categories := listboxCategories.Items.Strings[i] + '|';
      end
      else
      begin
        LogMetadataExt.Categories := listboxCategories.Items.Strings[i];
      end;
    end;



    // Pass the record over to formLogbook
    try
      if not DirectoryExists(returnDocumentsPath + '\TimberLog\') then  // Check if TimberLog user directory exists
      begin
        MkDir(returnDocumentsPath + '\TimberLog\');
      end;
    finally
      SaveDialog1.InitialDir := returnDocumentsPath + '\TimberLog';
    end;

    if SaveDialog1.Execute then
    begin
      formStartDialog.Visible := False;             // Hide the calling form first
      filepath := ChangeFileExt(SaveDialog1.FileName, '.logb');


      // Add to out Most recently used files
      mruMgr.IniFileName := GetAppConfigDir(True) + 'Test.ini';
      mruMgr.IniSection := 'RecentLogbooks';
      mruMgr.MaxRecent := 9;
      mruMgr.AddToRecent(filepath);


      //ShowMessage(filepath);

      newFormLogbook := TformLogbook.Create(Nil, filepath, lblTypepath.Caption, LogMetadataExt); // Use this format when making a new logbook

      newFormLogbook.ShowModal;                     //newFormLogbook is displayed
      FreeAndNil(newFormLogbook);                   //Free newFormLogbook

    end;
  end;



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

procedure TformStartDialog.Button3Click(Sender: TObject);
var
  cryptFile, keyString : String;
begin

  keyString := 'abcdef';
  cryptFile := 'Test.logb';
  dmCrypt.encryptFile(keyString, cryptFile);
  //dmCrypt.decryptFile(keyString, cryptFile);

end;

procedure TformStartDialog.chkAllowCategoriesChange(Sender: TObject);
begin
  if chkAllowCategories.Checked then
  begin
    chkAllowAddCategories.Enabled := True;
    txtCategory.Enabled := True;
    btnAddCategory.Enabled := True;
    btnDeleteCategory.Enabled := True;
    listboxCategories.Enabled := True;
  end
  else
  begin
    chkAllowAddCategories.Enabled := False;
    chkAllowAddCategories.Checked := False;
    txtCategory.Enabled := False;
    btnAddCategory.Enabled := False;
    btnDeleteCategory.Enabled := False;
    listboxCategories.Enabled := False;
  end;
end;

procedure TformStartDialog.chkPassMasterChange(Sender: TObject);
begin
  if chkPassMaster.Checked then
  begin
    txtMasterPass.Enabled := True;
    txtMasterPass1.Enabled := True;
  end
  else
  begin
    txtMasterPass.Enabled := False;
    txtMasterPass1.Enabled := False;
    txtMasterPass.Text := '';
    txtMasterPass1.Text := '';
  end;
end;

procedure TformStartDialog.chkPassToExportChange(Sender: TObject);
begin
  if chkPassToExport.Checked then
  begin
    txtPassToExport.Enabled := True;
    txtPassToExport1.Enabled := True;
  end
  else
  begin
    txtPassToExport.Enabled := False;
    txtPassToExport1.Enabled := False;
    txtPassToExport.Text := '';
    txtPassToExport1.Text := '';
  end;
end;

procedure TformStartDialog.chkPassToPrintChange(Sender: TObject);
begin
  if chkPassToPrint.Checked then
  begin
    txtPassToPrint.Enabled := True;
    txtPassToPrint1.Enabled := True;
  end
  else
  begin
    txtPassToPrint.Enabled := False;
    txtPassToPrint1.Enabled := False;
    txtPassToPrint.Text := '';
    txtPassToPrint1.Text := '';
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

procedure TformStartDialog.pgNewLogbookBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin
  lblBack.Visible := True;
  resetPages;
end;

procedure TformStartDialog.pgNewTypesBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin
  lblBack.Visible := False;
end;

procedure TformStartDialog.pgOpenLogbookBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin
  lblBack.Visible := True;
end;

// Create a new TformLogbook and pass the filepath to it
procedure TformStartDialog.ShellListView1DblClick(Sender: TObject);
var
  newFormLogbook : TformLogbook;
begin

  // Add to out Most recently used files
  mruMgr.IniFileName := GetAppConfigDir(True) + 'Test.ini';
  mruMgr.IniSection := 'RecentLogbooks';
  mruMgr.MaxRecent := 9;
  mruMgr.AddToRecent(ShellListView1.GetPathFromItem(ShellListView1.Selected));

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

procedure TformStartDialog.TabControl1Change(Sender: TObject);
begin
  Notebook2.PageIndex:=TabControl1.TabIndex;
  lblNow.Caption := FormatDateTime(cmbDFormat.Caption + ' ' + cmbTFormat.Caption, Now);
end;


// NOT GUI SPECIFIC. THIS SHOULD BE HANDLED ELSEWHERE (another unit). PASS memoDetails TO THE PROCEDURE
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

