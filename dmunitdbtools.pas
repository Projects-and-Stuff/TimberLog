unit dmUnitDBTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn, Sqlite3DS, FileUtil, Controls,
  RichMemo, unitClassLogbook, dmUnitCrypt, SynEdit, unitDefinitions;

type

  { TdmDBTools }

  TdmDBTools = class(TDataModule)
    DataSource1: TDataSource;
    SQLite3Connection1: TSQLite3Connection;
    Sqlite3Dataset1: TSqlite3Dataset;
    SQLQuery: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
    procedure createTableEntries;
    procedure createTableSettings;
    procedure createTableCategories;
    procedure createTableTemplates;
    procedure createTableUsers;
    procedure fillTableSettings(inputLogbook : TLogbook);
    procedure setAppId;                                      // Inserts an ID into pragma application_id
    procedure setUserVersion;                                // Inserts a random number into pragma user_verion

  public
    { public declarations }

    procedure createDatabase(inputLogbook : TLogbook; pass: String = ''); // Creates the tables, sets pragma details
    procedure closeAll;

    /// NOTE: Each of the search procedures includes default values. So to get all records from the table, just leave the inputs blank
    procedure entriesSearch(inputObject : TObject; searchString : String = '%'; category : String = '%'; username : String = '%'; dateStart : LongInt = 0; dateEnd : LongInt = 4102444800);
    procedure entriesNew(username : String; entry : String; lateEntry : LongInt = 0);
    // procedure categoriesNew
    // procedure categoriesSearch
    // procedure usersSearch
    // procedure usersAdd // Only adds one user at a time
    // procedure usersUpdatePass // Only updates one record at a time
    // procedure templatesSearch
    // procedure templatesAdd
    // procedure settingsSearch
    // procedure settingsAdd // Only used when creating the logbook (check whether the settings exist before trying to add)
    // procedure settingsUpdateChecksum
    // procedure settingsUpdatePassMaster
    // procedure settingsUpdatePassExport
    // procedure settingsUpdatePassPrint
  end;

var
  dmDBTools: TdmDBTools;

implementation

{$R *.lfm}



{ TdmDBTools }

procedure TdmDBTools.createDatabase(inputLogbook: TLogbook; pass: String); // Need password, too
var
  newFile : Boolean;
  tempStr : String;
begin
  SQLiteLibraryName := 'sqlite3.dll';

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  if not DirectoryExists(ExtractFilePath(inputLogbook.Path)) then
  begin
    try
      MkDir(ExtractFilePath(inputLogbook.Path)); // Make sure the directory exists before moving forward
    except
      inputLogbook.isError:='Unable to create specified logbook directory.';
    end;
  end;

  SQLite3Connection1.DatabaseName := inputLogbook.Path; // Set the database path
  Sqlite3Dataset1.FileName := inputLogbook.Path; // Set the database path
  try
    // Set the database password
    if pass = '' then
    begin
      SQLite3Connection1.Password := '';
    end
    else
    begin
      dmCrypt.stringHash(inputLogbook.PassMasterSalt + pass, tempStr);
      SQLite3Connection1.Password := tempStr;
    end;

    newFile := not FileExists(SQLite3Connection1.DatabaseName); // Check that the file doesn't already exist


    if newFile then
    begin


      // Make the database and the tables
      try
        SQLite3Connection1.Open;
        SQLTransaction1.Active := True;

        createTableEntries;
        createTableSettings;
        createTableCategories;
        createTableTemplates;
        createTableUsers;
        setUserVersion;
        setAppId;

        SQLTransaction1.Commit;
        //SQLTransaction1.Active := False;
        //SQLite3Connection1.Close;
      except
        inputLogbook.isError := 'Unable to generate underlying logbook structure. Please ensure you are making the logbook in a directory to which you have write permissions.';
      end;

      // Add the settings to the logbook
      try
        //SQLite3Connection1.Open;
        SQLite3Connection1.Connected := True;
        SQLTransaction1.Active := True;

        fillTableSettings(inputLogbook);



        SQLTransaction1.Commit;
        SQLTransaction1.StartTransaction;
        SQLTransaction1.Active := False;
        SQLite3Connection1.Connected := False;
        SQLite3Connection1.Close;
      except
        inputLogbook.isError := 'Unable to add settings values to logbook.';
      end;


    end
    else
    begin
      inputLogbook.isError := 'Logbook file already exists. Cannot overwrite. Please manually delete the old file first.';
    end;

  except
    // Error making database itself
    inputLogbook.isError := 'Unable to verify whether logbook file already exists.';
  end;



end;

procedure TdmDBTools.closeAll;
begin
  SQLTransaction1.Active := False;
  SQLite3Connection1.Close(True);
end;

procedure TdmDBTools.DataModuleCreate(Sender: TObject);
begin
  Randomize;
end;

procedure TdmDBTools.createTableEntries;
begin
  SQLite3Connection1.ExecuteDirect('CREATE TABLE "ENTRIES"('+
                    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
                    ' "DTActual" DateTime NOT NULL,'+
                    ' "DTIntended" DateTime NOT NULL,'+
                    ' "Checksum" Text NOT NULL,'+
                    ' "Username" Text NOT NULL,'+
                    ' "Category" Text,'+
                    ' "UUID" Text NOT NULL,'+
                    ' "Entry" Text NOT NULL);');
end;

procedure TdmDBTools.createTableSettings;
begin
  SQLite3Connection1.ExecuteDirect('CREATE TABLE "SETTINGS"('+
                    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
                    ' "DTOpened" DateTime NOT NULL,'+
                    ' "DTAccessed" DateTime NOT NULL,'+
                    ' "logName" Text NOT NULL,'+
                    ' "logDescription" Text NOT NULL,'+
                    ' "OpenedBy" Text NOT NULL,'+
                    //' "Checksum" Text NOT NULL,'+
                    ' "PassMasterSalt" Text,'+
                    ' "PassExportSalt" Text,'+
                    ' "PassExportHash" Text,'+
                    ' "PassPrintSalt" Text,'+
                    ' "PassPrintHash" Text,'+
                    ' "DTDisplayFormat" Text,'+
                    ' "PassPerUser" Bool NOT NULL,'+
                    ' "PassToExport" Bool NOT NULL,'+
                    ' "PassToPrint" Bool NOT NULL,'+
                    ' "AllowCategories" Bool NOT NULL,'+
                    ' "AllowAddCategories" Bool NOT NULL,'+
                    ' "AllowLateEntries" Bool NOT NULL);');

end;

procedure TdmDBTools.createTableCategories;
begin
  SQLite3Connection1.ExecuteDirect('CREATE TABLE "CATEGORIES"('+
                    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
                    ' "Category" Text NOT NULL);');
end;

procedure TdmDBTools.createTableTemplates;
begin
  SQLite3Connection1.ExecuteDirect('CREATE TABLE "TEMPLATES"('+
                    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
                    ' "TemplateType" Text NOT NULL,'+
                    ' "TemplateText" Text NOT NULL);');
end;

procedure TdmDBTools.createTableUsers;
begin
  SQLite3Connection1.ExecuteDirect('CREATE TABLE "USERS"('+
                    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
                    ' "Username" Text NOT NULL,'+
                    ' "PassSalt" Text NOT NULL,'+
                    ' "PassHash" Text NOT NULL);');
end;

procedure TdmDBTools.fillTableSettings(inputLogbook : TLogbook);
begin

  with Sqlite3Dataset1 do
  begin
    TableName := 'SETTINGS';
    Open;
    Append;

    FieldByName('DTOpened').AsDateTime := Now;
    FieldByName('DTAccessed').AsDateTime := Now;
    FieldByName('logName').AsString := inputLogbook.logName;
    FieldByName('logDescription').AsString := inputLogbook.logDescription;
    FieldByName('OpenedBy').AsString := inputLogbook.OpenedBy;
    //FieldByName('Checksum').AsString := inputLogbook.checksum;
    FieldByName('PassMasterSalt').AsString := inputLogbook.PassMasterSalt;
    FieldByName('PassExportSalt').AsString := inputLogbook.PassExportSalt;
    FieldByName('PassExportHash').AsString := inputLogbook.PassExportHash;
    FieldByName('PassPrintSalt').AsString := inputLogbook.PassPrintSalt;
    FieldByName('PassPrintHash').AsString := inputLogbook.PassPrintHash;
    FieldByName('DTDisplayFormat').AsString := inputLogbook.DTDisplayFormat;
    FieldByName('PassPerUser').AsBoolean := inputLogbook.PassPerUser;
    FieldByName('PassToExport').AsBoolean := inputLogbook.PassToExport;
    FieldByName('PassToPrint').AsBoolean := inputLogbook.PassToPrint;
    FieldByName('AllowCategories').AsBoolean := inputLogbook.AllowCategories;
    FieldByName('AllowAddCategories').AsBoolean := inputLogbook.AllowAddCategories;
    FieldByName('AllowLateEntries').AsBoolean := inputLogbook.AllowLateEntries;

    Post;
    ApplyUpdates;
    Close;
  end;

end;

// Inserts a random number into pragma user_verion
procedure TdmDBTools.setUserVersion;
begin
  SQLite3Connection1.ExecuteDirect('PRAGMA user_version = ' + IntToStr(Random(2000000000)) + ';');
end;

// Inserts an ID into pragma application_id
// PER SQLite Docs: Applications that use SQLite as their application file-format should set the Application ID integer to a unique integer
// Each release which changes the logbook file format in any way will use an
// incremented value for application_id, which will ensure no document conflicts
// (or that we can update old formats to the newest format)
procedure TdmDBTools.setAppId;
begin
  SQLite3Connection1.ExecuteDirect('PRAGMA application_id = ' + AppId + ';');
end;

// If searchString is left blank, it defaults to wildcard (pulls all records)
// searchString should be split up somehow!!!! Yet it should still avoid SQL injection
// If dateStart is left blank, it defaults to the epoch
// If dateEnd is left blank, it defaults to Jan 1, 2100
///////// Add search by Username and Category
procedure TdmDBTools.entriesSearch(inputObject: TObject;
  searchString: String; category: String; username: String; dateStart: LongInt;
  dateEnd: LongInt);
var
  ActUpon : TRichMemo;
  tempSearchString : String;
  SdateStart, SdateEnd : String;
begin

  ActUpon := TRichMemo(inputObject);
  ActUpon.Clear;

  // use tempSearchString to split searchString up into its individual parts and apply each term to the search





  SdateStart := FormatDateTime('yyyy-mm-dd', + TDate(FileDateToDateTime(dateStart))); // Convert to format SQLite can interpret
  SdateEnd := FormatDateTime('yyyy-mm-dd', + TDate(FileDateToDateTime(dateEnd))); // Convert to format SQLite can interpret

  with SQLQuery do
  begin
    Close;
    SQL.Text := 'Select * FROM Entries WHERE id > 1 AND (Entry LIKE :searchString OR Name LIKE :searchString) AND (strftime(' + QuotedStr('%Y-%m-%d') + ', Intended_DT) >= date(:dateStart)) AND (strftime(' + QuotedStr('%Y-%m-%d') + ', Intended_DT) >= date(:dateEnd))';
    Prepare;
    ParamByName('searchString').AsString := '%' + searchString + '%';
    ParamByName('dateStart').AsString := SdateStart;
    ParamByName('dateEnd').AsString := SdateEnd;
    //ParamByName('category').AsString := SdateEnd;
    //ParamByName('username').AsString := SdateEnd;
    Open;

    while not EOF do
    begin
      ActUpon.Lines.AddText('Name: ' + FieldByName('Name').AsString + ', usernum: ' + FieldByName('usernum').AsString);
      Next;
    end;
  end;




  // Iterate through the results

end;

// Adds a new entry to the logbook. If no lateEntry date is provided, it defaults to 0 and 'Now()' is used in the logbook
procedure TdmDBTools.entriesNew(username: String; entry: String;
  lateEntry: LongInt);
begin


end;



end.

