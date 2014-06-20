unit unitStartFunctions;

// Provides all the necessary functionality to allow
// formUnitStartDialog to focus on the GUI

// 1. Loads Logbook types and passes back to formUnitStartDialog
// 2. Reads records from any selected logbook and returns the record to formUnitStartDialog
// 3. When creating a new logbook, passes the filenae and record to formLogbook
// 4. When opening a logbook, passes the filenae formLogbook

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitRecordLogMetadata, shlobj;


  function buildLogMetadataRecord:RLogMetadata; // Processes data from Page 1 and creates RLogMetadata record
  procedure resetPages; // Resets Page 1 to the default values
  function readLogMetadata(path : String):RLogMetadata;
  function returnDocumentsPath : String;
  function verifyLogMetadata(LogMetadata : RLogMetadata):Boolean;


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





end.

