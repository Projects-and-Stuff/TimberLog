unit unitStartFunctions;

// Provides all the necessary functionality to allow
// formUnitStartDialog to focus on the GUI

// 1. Loads Logbook types and passes back to formUnitStartDialog
// 2. Reads Metadata record from any selected logbook and returns the record to formUnitStartDialog
// 3. When creating a new logbook, passes the filename and Metadata record to formLogbook
// 4. When opening a logbook, passes the filename to formLogbook

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, shlobj, unitRecordLogMetadata, strutils;


  function buildLogMetadataRecord:RLogMetadata; // Processes data from Page 1 and creates RLogMetadata record
  procedure resetPages; // Resets Page 1 to the default values
  function readLogMetadata(path : String):RLogMetadata;
  function returnDocumentsPath : String;
  function verifyLogMetadata(LogMetadata : RLogMetadata):Boolean;
  procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type : String; Description : TStringList); // Short form used to identify the type name and description
  procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type : String; Description : TStringList;  // Short form used to identify the type name, description, and categories
    Default_Categories : TStringList);
  procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type : String; Description : TStringList; // Full form with entire type definition
    Default_Categories: TStringList; Default_Text: TStringList;
    One_Line: TStringList; Multi_Line: TStringList);
  procedure getTypeFileList(var fileList : TStringList);

implementation

uses
  formUnitStartDialog;

function buildLogMetadataRecord: RLogMetadata;
begin


end;

procedure resetPages;
begin
  formStartDialog.Notebook1.PageIndex := 0; // Reset to the default page on formStartDialog

  // Clear out all the components on page 1






  // Reset the path for ShellTreeView on page 2
  try
    if not DirectoryExists(returnDocumentsPath + '\TimberLog\') then  // Check if TimberLog user directory exists
    begin
      MkDir(returnDocumentsPath + '\TimberLog\');
    end;


    formStartDialog.ShellTreeView1.Path := returnDocumentsPath + '\TimberLog\';
  finally

  end;

end;

function readLogMetadata(path: String): RLogMetadata;
var
  SettingsValid : Boolean;
  BytesRead : Int64;
  LogMetadata : RLogMetadata;
  FSRecord: TFileStream;
begin
  SettingsValid := False;

  try
    FSRecord := TFileStream.Create(path, fmOpenRead);
      try
        FSRecord.Seek(FSRecord.Size - SizeOf(LogMetadata), soBeginning);  // Move to beginning of the record in the file
        BytesRead := FSRecord.Read(LogMetadata,sizeof(LogMetadata)); // Read the record


        if verifyLogMetadata(LogMetadata) = True then
        begin
          SettingsValid := True;
        end;

      finally
        FSRecord.Free; // Save new file to disk and free stream
      end;
  finally

  end;

  if SettingsValid = True then
  begin

  end
  else
  begin
    LogMetadata.OpenedBy := 'The Logbook Is Corrupt';
    LogMetadata.footerMark := '00000000000';
  end;

  readLogMetadata := LogMetadata; // Return the record

end;

function returnDocumentsPath: String;
var
  DocumentsPath: Array[0..MaxPathLen] of Char; //Allocate memory
begin
  DocumentsPath := '';
  SHGetSpecialFolderPath(0,DocumentsPath,CSIDL_PERSONAL,false); // http://delphi-miranda-plugins.googlecode.com/svn-history/r105/trunk/FPC/units/src/shlobj.pp
  returnDocumentsPath := DocumentsPath;
end;

function verifyLogMetadata(LogMetadata: RLogMetadata): Boolean;
begin
  if LogMetadata.footerMark = '>TimberLog<' then
  begin
    verifyLogMetadata := True;
  end
  else
  begin
    verifyLogMetadata := False;
  end;
end;

procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type: String; Description: TStringList);
var
  utilStrList : TStringList;

  // These loc arrays are used for identifying the type data
  // The first value (IE: locType[0]) counts how many instances there are so far
  // All additional values specify the location of the blocks
  locType, locDesc : array of Integer;

  i : Integer;
  hasError : Boolean; // Used to check whether there is an error in number of block elements
const
  blockLogbook_Type = '>>>*Logbook_Type*<<<';
  blockDescription = '>>>*Description*<<<';
begin
  utilStrList := TStringList.Create;
  utilStrList.LoadFromFile(filepath); // Load up the file into the utility TStringList

  // Set the initial length of locating arrays
  SetLength(locType, 1);
  SetLength(locDesc, 1);

  // Initially, there are no instances of each type block
  locType[0] := 0;
  locDesc[0] := 0;


  // Initially, run through the entire file looking for the blocks
  for i := 0 to utilStrList.Count-1 do
  begin
    if (AnsiStartsText(blockLogbook_Type, utilStrList.Strings[i]) = true) then
    begin
      locType[0] := locType[0] + 1; // If we've got a match, increase the count
      SetLength(locType, Length(locType)+1); // Increase the array length
      locType[locType[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    if (AnsiStartsText(blockDescription, utilStrList.Strings[i]) = true) then
    begin
      locDesc[0] := locDesc[0] + 1; // If we've got a match, increase the count
      SetLength(locDesc, Length(locDesc)+1); // Increase the array length
      locDesc[locDesc[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    // Add further block element searches here...
  end;

  // Check to ensure each block has an even number of elements
  hasError := False;
  errorMsg := '';

  if ((Length(locType)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockLogbook_Type + '" element of this Logbook Type file';
    hasError := True;
  end;

  if ((Length(locType)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockDescription + '" element of this Logbook Type file';
    hasError := True;
  end;



  // Then process the blocks into their respective variables (String or TStringList)
  if errorMsg = '' then // If there's no error, keep on processing
  begin

      // Logbook_Type is a special case since only one entry is allowed, and only one line is allowed
      Logbook_Type := utilStrList.Strings[locType[1]+1];

      // Add the description, which is a single multi-line block (additional intances of Description are ignored)
      for i := locDesc[1]+1 to locDesc[2]-1 do
      begin
        Description.AddText(utilStrList.Strings[i]);
      end;


  end;



  utilStrList.Free;
end;

procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type: String; Description: TStringList;
  Default_Categories: TStringList);
begin

end;

procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type: String; Description: TStringList;
  Default_Categories: TStringList; Default_Text: TStringList;
  One_Line: TStringList; Multi_Line: TStringList);
begin

end;

procedure getTypeFileList(var fileList: TStringList);
var
  Info : TSearchRec;
  anyExists : Boolean;
begin


  anyExists := False;

  if not DirectoryExists(GetAppConfigDir(True)) then // Check if app config directory (C:\ProgramData\TimberLog\) exists
  begin
    try
      MkDir(GetAppConfigDir(True)); // if not, create it
    finally
    end;
  end;


  if FindFirst(GetAppConfigDir(True) + '*', faAnyFile and faDirectory, Info) = 0 then
  begin
    Repeat

      With Info do
      begin
        //If it's a .logt file, then add it to the list
        if (RightStr(Name, 4) = 'logt') then
        begin
          //fileList.AddText(GetAppConfigDir(True) + Name);
          anyExists := True;
          fileList.AddText(GetAppConfigDir(True) + Name);
        end;
      end;

    Until FindNext(Info)<>0;
  end;

  if anyExists = False then
  begin
    fileList.AddText('None');
  end;


  FindClose(Info); // Close our search

  //fileList.Append('-');
end;





end.

