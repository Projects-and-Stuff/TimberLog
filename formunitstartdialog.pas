unit formUnitStartDialog;

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

// GUI-related code for formStartDialog

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  types, LCLType, ExtCtrls, Buttons, ShellCtrls, LCLIntf, IniPropStorage,
  formUnitLogbook, unitDefinitions, unitStartFunctions, unitRecordLogMetadata,
  ComCtrls, Grids, dmUnitCrypt, dmUnitDBTools, unitTypeTile, unitRecentTile,
  mrumanager, UniqueInstance, strutils, Contnrs, unitClassLogbook;

type

  { TformStartDialog }

  TformStartDialog = class(TForm)
    btnAddCategory: TButton;
    btnDeleteCategory: TButton;
    Button1: TButton;
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
    lblCreate: TLabel;
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
    shellListSelectFile: TShellListView;
    shellTreeSelectFolder: TShellTreeView;
    btnClose: TSpeedButton;
    btnHelp: TSpeedButton;
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
    UniqueInstance1: TUniqueInstance;
    procedure LabelAsButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LabelAsButtonMouseLeave(Sender: TObject);
    procedure LabelAsButtonMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelAsButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnAddCategoryClick(Sender: TObject);
    procedure btnDeleteCategoryClick(Sender: TObject);
    procedure chkAllowCategoriesChange(Sender: TObject);
    procedure chkPassMasterChange(Sender: TObject);
    procedure chkPassToExportChange(Sender: TObject);
    procedure chkPassToPrintChange(Sender: TObject);
    procedure cmbDFormatChange(Sender: TObject);
    procedure cmbTFormatChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblCreateClick(Sender: TObject);
    procedure lblBackClick(Sender: TObject);
    procedure lblOpenOtherLogbooksClick(Sender: TObject);
    procedure pgNewLogbookBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure pgNewTypesBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure pgOpenLogbookBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure shellListSelectFileDblClick(Sender: TObject);
    procedure shellListSelectFileSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnCloseClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
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

    shellTreeSelectFolder.Path := returnDocumentsPath + '\TimberLog\';
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

  if Assigned(recentTileList) then // Check if we've already created the recentTiles
  begin
    recentTileList.Clear; // If so, remove them all in prep for refreshing the list
  end;

  if Assigned(typeTileList) then // Check if we've already created the typeTiles
  begin
    typeTileList.Clear; // If so, remove them all in prep for refreshing the list
  end;

  recentTileList := TObjectList.create(); // Make the object lists for our frames
  typeTileList := TObjectList.create();

  mruMgr.IniFileName := GetAppConfigDir(True) + '\TimberLog.ini'; // Get our recent files
  mruMgr.IniSection := 'RecentLogbooks';
  mruMgr.MaxRecent := 9;
  mruMgr.ShowRecentFiles;

  if mruMgr.Recent.Count > 0 then
  begin
    for i := 0 to mruMgr.Recent.Count-1 do
    begin
      recentTileList.Add(TframeRecentTile.Create(formStartDialog));
    end;
    for i := mruMgr.Recent.Count-1 downto 0 do
    begin
      TframeRecentTile(recentTileList.Items[i]).Parent := ScrollBox2;
      TframeRecentTile(recentTileList.Items[i]).Align := alTop;
      TframeRecentTile(recentTileList.Items[i]).setColorInitial(clWhite);
      TframeRecentTile(recentTileList.Items[i]).setColorLeave(clWhite);
      TframeRecentTile(recentTileList.Items[i]).setColorEnter(clbBGMouseOver);
      TframeRecentTile(recentTileList.Items[i]).setColorDown(clbBGClicked);
      TframeRecentTile(recentTileList.Items[i]).setColorUp(clbBGMouseOver);
      TframeRecentTile(recentTileList.Items[i]).setFilename(ExtractFileNameOnly(mruMgr.Recent.Strings[i]));
      TframeRecentTile(recentTileList.Items[i]).setPath(mruMgr.Recent.Strings[i]);
    end;
  end
  else
  begin
    ShowMessage('No recent files');
  end;

  fileList := TStringList.Create;
  getTypeFileList(fileList);
  fileList.Sort;

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
    for i := 0 to fileList.Count-1 do
    begin
      typeTileList.Add(TframeTypeTile.Create(formStartDialog));
    end;
    for i := fileList.Count-1 downto 0 do
    begin
      Description := TStringList.Create;
      processTypeFile(fileList[i], errorMsg, Logbook_Type, Description);

      TframeTypeTile(typeTileList.Items[i]).Parent := ScrollBox1;
      TframeTypeTile(typeTileList.Items[i]).Align := alTop;
      TframeTypeTile(typeTileList.Items[i]).setColorInitial(clbBGDefault);
      TframeTypeTile(typeTileList.Items[i]).setColorLeave(clbBGDefault);
      TframeTypeTile(typeTileList.Items[i]).setColorEnter(clbBGMouseOver);
      TframeTypeTile(typeTileList.Items[i]).setColorDown(clbBGClicked);
      TframeTypeTile(typeTileList.Items[i]).setColorUp(clbBGMouseOver);
      TframeTypeTile(typeTileList.Items[i]).setTitle(Logbook_Type);
      TframeTypeTile(typeTileList.Items[i]).setDescription(Description.Strings[0]);
      TframeTypeTile(typeTileList.Items[i]).setPath(fileList.Strings[i]);
      TframeTypeTile(typeTileList.Items[i]).DescriptionStrings.Strings := Description;
      Description.Free;
    end;
  end;

  fileList.Free;
  Splitter1.Color := clbMedium;
  Splitter2.Color := clbMedium;
  Shape1.Pen.Color := clbMedium;
  lblOpenOtherLogbooks.Font.Color := clbVividTextDefault;
  lblBack.Font.Color := clbVividTextDefault;
  lblCreate.Font.Color := clbVividTextDefault;
  shellListSelectFile.Font.Color := clbVividTextDefault;
  pgNewLogbook.Color := clbBGDefault;

  shellListSelectFile.Mask := '*.logb'; // Only display *.logb files
  Notebook1.PageIndex := 0;

end;

procedure TformStartDialog.lblCreateClick(Sender: TObject);
var
  sendLogbook : TLogbook;
  newFormLogbook : TformLogbook;
  filepath : String;
  i : Integer;
  hasError : Boolean;
  tempStr : String;
begin
  sendLogbook := TLogbook.Create;


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
    // Set the values in the logbook from the form
    sendLogbook.logName := txtLogName.Text;
    sendLogbook.logDescription := memoLogDescription.Text;
    sendLogbook.OpenedBy := txtOpenedBy.Text;
    sendLogbook.DTOpened := DateTimeToFileDate(Now);
    sendLogbook.DTAccessed := DateTimeToFileDate(Now);
    sendLogbook.PassMaster := chkPassMaster.Checked;
    sendLogbook.PassPerUser := chkPassPerUser.Checked;
    sendLogbook.PassToExport := chkPassToExport.Checked;
    sendLogbook.PassToPrint := chkPassToPrint.Checked;
    dmCrypt.stringhash(txtMasterPass.Text, tempStr);   // Hashed password
    sendLogbook.PassMasterHash := tempStr;
    dmCrypt.stringhash(txtPassToExport.Text, tempStr); // Hashed password
    sendLogbook.PassExportHash := tempStr;
    dmCrypt.stringhash(txtPassToPrint.Text, tempStr);   // Hashed password
    sendLogbook.PassPrintHash := tempStr;
    sendLogbook.AllowCategories := chkAllowLateEntries.Checked;
    sendLogbook.AllowAddCategories := chkAllowAddCategories.Checked;
    sendLogbook.AllowLateEntries := chkAllowLateEntries.Checked;
    sendLogbook.DTDisplayFormat:=Trim(cmbDFormat.Caption + ' ' + cmbTFormat.Caption);
    // Generate salts
    sendLogbook.PassMasterSalt := dmCrypt.newSalt();
    sendLogbook.PassExportSalt := dmCrypt.newSalt();
    sendLogbook.PassPrintSalt := dmCrypt.newSalt();


    // Convert the contents of the Categories listbox to a delimited string
    for i := 0 to listboxCategories.Count-1 do
    begin
      ReplaceText(listboxCategories.Items.Strings[i], '|', ':');
      if not i = listboxCategories.Count-1 then
      begin
        sendLogbook.Categories := listboxCategories.Items.Strings[i] + '|';
      end
      else
      begin
        sendLogbook.Categories := listboxCategories.Items.Strings[i];
      end;
    end;


    // Allow user to select a location to save the file
    try
      if not DirectoryExists(returnDocumentsPath + '\TimberLog\') then  // Check if TimberLog user directory exists
      begin
        MkDir(returnDocumentsPath + '\TimberLog\');
      end;
      SaveDialog1.FileName := returnDocumentsPath + '\TimberLog\' + sendLogbook.logName + '.logb';
    finally
      //SaveDialog1.InitialDir := returnDocumentsPath + '\TimberLog';
      //SaveDialog1.FileName := returnDocumentsPath + '\TimberLog\' + sendLogbook.logName + '.logb';
    end;

    //SelectDirectoryDialog1.InitialDir := returnDocumentsPath + '\TimberLog\';
    //SelectDirectoryDialog1.Title := 'Choose Location to Save the Logbook';

    if SaveDialog1.Execute then
    //if SelectDirectoryDialog1.Execute then
    begin


      filepath := ChangeFileExt(SaveDialog1.FileName, '.logb');
      //filepath := ChangeFileExt(IncludeTrailingBackslash(SelectDirectoryDialog1.FileName) + sendLogbook.logName, '.logb');

      formStartDialog.Visible := False;             // Hide the calling form first

      // Add to our Most recently used files
      mruMgr.ShowRecentFiles;
      mruMgr.AddToRecent(filepath);


      //ShowMessage(filepath);

      sendLogbook.Path := filepath;
      dmDBTools.createDatabase(sendLogbook);

      newFormLogbook := TformLogbook.Create(Nil, sendLogbook); // Use this format when making a new logbook

      newFormLogbook.ShowModal;                     //newFormLogbook is displayed
      FreeAndNil(newFormLogbook);                   //Free newFormLogbook
      FreeAndNil(sendLogbook);



      {
      //////////////// THIS IS THE TEST START ////////////////
      filepath := ChangeFileExt(SaveDialog1.FileName, '.logb');
      sendLogbook.Path := filepath;

      dmDBTools.createDatabase(sendLogbook);

      ShowMessage(sendLogbook.isError);
      FreeAndNil(sendLogbook);

      ///////////////// THIS IS THE TEST END ////////////////
      }



    end;
  end;



end;

procedure TformStartDialog.lblBackClick(Sender: TObject);
begin

  pgNewTypes.Show;
  Notebook1.PageIndex := 0;
  lblBack.Visible := False;
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
  IniPropStorage1.IniFileName := GetAppConfigDir(True) + '\TimberLog.ini';
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
procedure TformStartDialog.shellListSelectFileDblClick(Sender: TObject);
var
  newFormLogbook : TformLogbook;
begin

  // Add to our Most Recently Used (MRU) file list
  mruMgr.ShowRecentFiles;
  mruMgr.AddToRecent(shellListSelectFile.GetPathFromItem(shellListSelectFile.Selected));

  formStartDialog.Visible := False;             // Hide the calling form first
  newFormLogbook := TformLogbook.Create(Nil, shellListSelectFile.GetPathFromItem(shellListSelectFile.Selected));

  newFormLogbook.ShowModal;                     //Form2 is displayed
  FreeAndNil(newFormLogbook);                   //Free Form2
end;

procedure TformStartDialog.shellListSelectFileSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  tempLogbook : TLogbook;
  LogMetadata : RLogMetadata;
begin
  tempLogbook := TLogbook.Create;

  try
    tempLogbook.Path := shellListSelectFile.GetPathFromItem(Item);
    tempLogbook.readLogMetadata;
    tempLogbook.WriteMetadataToMemo(memoDetails); // Writes the record contents to the memo
  finally
    tempLogbook.Free;
  end;

end;

procedure TformStartDialog.btnCloseClick(Sender: TObject);
begin
  formStartDialog.Close;
end;

procedure TformStartDialog.btnHelpClick(Sender: TObject);
begin
  OpenDocument('TimberLogHelp\index.html');
end;

procedure TformStartDialog.TabControl1Change(Sender: TObject);
begin
  Notebook2.PageIndex:=TabControl1.TabIndex;
  lblNow.Caption := FormatDateTime(cmbDFormat.Caption + ' ' + cmbTFormat.Caption, Now);
end;


end.

