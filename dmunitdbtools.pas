unit dmUnitDBTools;

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
  Classes, SysUtils, db, sqldb, sqlite3conn, Sqlite3DS, FileUtil, Controls,
  RichMemo, unitClassLogbook, dmUnitCrypt, SynEdit, unitDefinitions;

type

  { TdmDBTools }

  TdmDBTools = class(TDataModule)
    DataSource1: TDataSource;
    SQLite3Connection1: TSQLite3Connection;
    Sqlite3Dataset1: TSqlite3Dataset;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
    procedure createTableEntries;                            // Creates the Entries Table
    procedure createTableSettings;                           // Creates the Entries Table
    procedure createTableCategories;                         // Creates the Entries Table
    procedure createTableTemplates;                          // Creates the Entries Table
    procedure createTableUsers;                              // Creates the Entries Table
    procedure settings_Add(inputLogbook : TLogbook);         // Initially fills in the values for the Settings Table (check whether the settings exist before trying to add)
    procedure settings_Update(inputLogbook : TLogbook);      // Updates writeable values for the Settings Table
    procedure setAppId;                                      // Inserts app ID into pragma application_id - Used to identify .logb file format version used
    procedure setUserVersion;                                // Inserts a random number into pragma user_verion
    procedure queryAppId(id : String);                       // Obtains the application_id Pragma value
    procedure queryUserVersion(version : String);            // Obtains the user_version Pragma value
    procedure resetKey(oldKey : String; newKey : String);
  public
    { public declarations }

    procedure createDatabase(inputLogbook : TLogbook; pass: String = ''); // Creates the tables, sets pragma details
    procedure closeAll;

    /// NOTE: Each of the search procedures includes default values. So to get all records from the table, just leave the inputs blank
    procedure entries_Search(inputObject : TObject; searchString : String = '%'; category : String = '%'; username : String = '%'; dateStart : LongInt = 0; dateEnd : LongInt = 4102444800);
    procedure entries_New(username : String; entry : String; lateEntry : LongInt = 0);
    procedure categories_Add(newCategory : String);
    procedure categories_Query(sort : Boolean);
    // procedure users_Query
    procedure users_Add(newUsername : String; passSalt : String; passHash : String; Question1 : String; Answer1 : String); // Only adds one user at a time
    procedure users_UpdatePass(oldPassHash : String; newPassHash : String); // Only updates one record at a time
    // procedure templates_Query
    procedure templates_Add(newTemplateItem : String);
    // procedure settings_Query
    // procedure settings_UpdateChecksum
    // procedure settings_UpdatePassMaster
    // procedure settings_UpdatePassExport
    // procedure settings_UpdatePassPrint
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

        settings_Add(inputLogbook);



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
  Randomize; // Make sure we can obtain good randomized values

  SQLiteLibraryName := 'sqlite3.dll'; // Ensure we're using the local sqlite3.dll
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
                    ' "Username" Text NOT NULL UNIQUE,'+
                    ' "PassSalt" Text NOT NULL,'+
                    ' "PassHash" Text NOT NULL,'+
                    ' "Question1" Text NOT NULL,'+
                    ' "Answer1" Text NOT NULL);');
end;

procedure TdmDBTools.settings_Add(inputLogbook : TLogbook);
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

// Updates the following values in the Settings Table. Used when closing the logbook
// - DTAccessed
// - Checksum
procedure TdmDBTools.settings_Update(inputLogbook: TLogbook);
begin

end;



// Inserts a random number into pragma user_verion
procedure TdmDBTools.setUserVersion;
begin
  SQLite3Connection1.ExecuteDirect('PRAGMA user_version = ' + IntToStr(Random(2000000000)) + ';');
end;

procedure TdmDBTools.queryAppId(id: String);
begin
  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  //SQLite3Connection1.Password := txtPass.Text; // The current password

  //SQLite3Connection1.DatabaseName := 'new.db'; // Set the path to the database

  // Try to query database for application_id Pragma
  try
    SQLite3Connection1.Open;

    SQLQuery1.SQL.Text := 'PRAGMA application_id;';
    SQLQuery1.Open;

    // Display the resulting value
    id := SQLQuery1.fields[0].asString;

  except
    // Error
  end;
end;

procedure TdmDBTools.queryUserVersion(version: String);
begin
  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  //SQLite3Connection1.Password := txtPass.Text; // The current password

  //SQLite3Connection1.DatabaseName := 'new.db'; // Set the path to the database

  // Try to query database for application_id Pragma
  try
    SQLite3Connection1.Open;

    SQLQuery1.SQL.Text := 'PRAGMA user_version;';
    SQLQuery1.Open;

    // Display the resulting value
    version := SQLQuery1.fields[0].asString;

  except
    // Error
  end;
end;

procedure TdmDBTools.resetKey(oldKey : String; newKey : String);
begin

    SQLite3Connection1.Close; // Ensure the connection is closed when we start

    SQLite3Connection1.Password := oldKey; // The current password

    //SQLite3Connection1.DatabaseName := 'new.db'; // Set the path to the database

    // Update the database key
    try
      SQLite3Connection1.Open;
      SQLTransaction1.Active := True;


      // Here we change the key.
      // We use double-quotes here so that a blank key (IE: "") can be provided if
      // you want to remove encryption from the database.
      // This is a very simplistic demonstration. Ideally, we would take a stronger cryptographic approach
      // Some helpful info on this topic can be found at:
      // https://www.owasp.org/index.php/Cheat_Sheets
      // Per SQLite Documentation:
      // Note that the hexkey, rekey and hexrekey pragmas only work with SQLite version 3.6.8 and later.
      // http://www.sqlite.org/see/doc/trunk/www/readme.wiki
      // Section: Using the "key" PRAGMA
      SQLite3Connection1.ExecuteDirect('PRAGMA rekey = "' + newKey + '";');


      SQLTransaction1.Commit;
      SQLTransaction1.Active := False;
      SQLite3Connection1.Close;

      // If we're going to store the key or hash, make sure we update the value here



    except
      // Return error
    end;
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
procedure TdmDBTools.entries_Search(inputObject: TObject;
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

  with SQLQuery1 do
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
procedure TdmDBTools.entries_New(username: String; entry: String;
  lateEntry: LongInt);
begin


end;

procedure TdmDBTools.categories_Add(newCategory: String);
begin

end;

procedure TdmDBTools.categories_Query(sort: Boolean);
begin

end;

procedure TdmDBTools.users_Add(newUsername: String; passSalt: String;
  passHash: String; Question1: String; Answer1: String);
begin

end;

procedure TdmDBTools.users_UpdatePass(oldPassHash: String; newPassHash: String);
begin

end;

procedure TdmDBTools.templates_Add(newTemplateItem: String);
begin

end;



end.

