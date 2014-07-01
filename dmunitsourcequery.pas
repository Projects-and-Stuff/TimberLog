unit dmUnitSourceQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, Sqlite3DS, FileUtil, Controls, RichMemo;

type

  { TdmSourceQuery }

  TdmSourceQuery = class(TDataModule)
    DataSource1: TDataSource;
    Sqlite3Dataset1: TSqlite3Dataset;
    SQLQuery: TSQLQuery;
  private
    { private declarations }
  public
    { public declarations }

    /// NOTE: Each of the search procedures includes default values. So to get all records from the table, just leave the inputs blank

    // procedure createDatabase // Creates the tables, sets pragma details
    procedure entriesSearch(inputObject : TObject; searchString : String = '%'; category : String = '%'; username : String = '%'; dateStart : LongInt = 0; dateEnd : LongInt = 4102444800);
    // procedure entriesNew
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
  dmSourceQuery: TdmSourceQuery;

implementation

{$R *.lfm}



{ TdmSourceQuery }


// If searchString is left blank, it defaults to wildcard (pulls all records)
// searchString should be split up somehow!!!! Yet it should still avoid SQL injection
// If dateStart is left blank, it defaults to the epoch
// If dateEnd is left blank, it defaults to Jan 1, 2100
///////// Add search by Username and Category
procedure TdmSourceQuery.entriesSearch(inputObject : TObject; searchString, category, username : String; dateStart : LongInt;
  dateEnd : LongInt);
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



end.

