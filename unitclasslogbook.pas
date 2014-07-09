unit unitClassLogbook;

// Class Unit for class Logbook

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitRecordLogMetadata, unitDefinitions, dmUnitCrypt, StdCtrls;

type

  { TLogbook }

  TLogbook = class(TObject)
    private // self access only
      FheaderMark : String;       // used to verify TLogMetadata
      FfooterMark : String;       // used to verify TLogMetadata
      FPath : String; 			// Full path to the logbook file (*.logb)
      FlogName : String;		// Serial Number or Name of equipment (Should match the value in Settings table)
      FlogDescription : String;		// Short description or directions for this logbook (Should match the value in Settings table)
      FOpenedBy : String;		// Name of person opening the logbook (Should match the value in Settings table)
      Fchecksum : String; 		// Checksum of the metadata settings
      FfileChecksum : String;           // Checksum of the entire File minus RLogMetadata)
      Fchecksumsum : String; 	        // Checksum of all entry checksums (This may just take too long to be reasonable)
      FDTOpened : LongInt;		// DateTime that the file was originally opened (Should match the value in Settings table)
      FDTAccessed : LongInt;		// DateTime that the file was last saved (Should match the value in Settings table)
      FPassMaster : Boolean;		// Is a master password required?
      FPassPerUser : Boolean;		// Are users required to use individual passwords to submit entries?
      FPassToExport : Boolean;		// Are users required to enter a password to export to text/pdf/other formats?
      FPassToPrint : Boolean;		// Are users required to enter a password to print the logbook?
      FPassMasterSalt : String;          // Salt for Master Password
      FPassMasterHash : String;          // Hashed Master Password
      FPassExportSalt : String;          // Salt for Export Password
      FPassExportHash : String;          // Hashed Export Password (Can be recovered using master password)
      FPassPrintSalt : String;           // Salt for Print Password
      FPassPrintHash : String;           // Hashed Print Password  (Can be recovered using master password)
      FAllowCategories : Boolean;	 // Are categories allowed in this logbook?
      FAllowAddCategories : Boolean;	 // Can users add to the list of categories (or are the categories constants?)
      FAllowLateEntries : Boolean;	 // Are users allowed to add late entries? (the logbook creator that the actual date is also saved)
      FCategories : String;              // The list of categories
      FDTDisplayFormat : String; 	 // Format string for the DateTime fields (internally stored as LongInt)
      FisError : String;                 // Tests whether the logbook is in a fault condition at any point during its creation

    public // access by anything
      LogMetadata: RLogMetadata;        // The Logbook Metadata record.
                                        // Its purpose is only for reading/writing metadata from/to the logbook file

      procedure writeMetadataToMemo(outMemo : TMemo);
      function verifyLogMetadataStructure(tempLogMetadata : RLogMetadata):Boolean ; // Verifies that FLogMetadata header and footer are valid
      function verifyLogMetadataContent(tempLogMetadata : RLogMetadata):Boolean ; // Verifies that FLogMetadata header and footer are valid
      procedure createBackup;            // Copies 'filepath', minus the record, and renames both files
      function readLogMetadata(tempRecord : Boolean = False): Boolean; // Reads the record from the end of 'filepath'
      procedure buildLogMetadataRecord(AlogName, AlogDescription, AOpenedBy : String; ADTOpened, ADTAccessed : LongInt); // Processes data from Page 1 and creates RLogMetadata record
      procedure writeLogMetadata;        // Writes FLogMetadata to file
      procedure deleteBackupLog;         // Deletes the backup logbook
      procedure copyDataToLogMetadata;         // Copies the data from the main class parameters to LogMetadata
      procedure copyDataFromLogMetadata;         // Copies the data from LogMetadata to the main class parameters
      constructor Create; overload;
      constructor Create(Args: array of Integer); overload;
      destructor Destroy; override;

      // Make OpenLogbook a function with boolean return
      procedure openLogbook; // Opens an existing logbook file

      // Make NewLogbook a function with boolean return
      procedure newLogbook; // Creates a new logbook file database
      procedure closeLogbook;            // Process for final logbook closing

    published // special type of public scope
      property Path: String read FPath write FPath; // Allows access for reading and writing Path of the logbook
      property headerMark: String read FheaderMark write FheaderMark; // Allows access for reading and writing Path of the logbook
      property footerMark: String read FfooterMark write FfooterMark; // Allows access for reading and writing Path of the logbook
      property logName: String read FlogName write FlogName; // Allows access for reading and writing Path of the logbook
      property logDescription: String read FlogDescription write FlogDescription; // Allows access for reading and writing Path of the logbook
      property OpenedBy: String read FOpenedBy write FOpenedBy; // Allows access for reading and writing Path of the logbook
      property checksum: String read Fchecksum write Fchecksum; // Allows access for reading and writing Path of the logbook
      property fileChecksum: String read FfileChecksum write FfileChecksum; // Allows access for reading and writing Path of the logbook
      property checksumsum: String read Fchecksumsum write Fchecksumsum; // Allows access for reading and writing Path of the logbook
      property DTOpened: LongInt read FDTOpened write FDTOpened; // Allows access for reading and writing Path of the logbook
      property DTAccessed: LongInt read FDTAccessed write FDTAccessed; // Allows access for reading and writing Path of the logbook
      property PassMaster: Boolean read FPassMaster write FPassMaster; // Allows access for reading and writing Path of the logbook
      property PassPerUser: Boolean read FPassPerUser write FPassPerUser; // Allows access for reading and writing Path of the logbook
      property PassToExport: Boolean read FPassToExport write FPassToExport; // Allows access for reading and writing Path of the logbook
      property PassToPrint: Boolean read FPassToPrint write FPassToPrint; // Allows access for reading and writing Path of the logbook
      property PassMasterSalt : String read FPassMasterSalt write FPassMasterSalt; // Allows access for reading and writing Path of the logbook
      property PassMasterHash : String read FPassMasterHash write FPassMasterHash; // Allows access for reading and writing Path of the logbook
      property PassExportSalt : String read FPassExportSalt write FPassExportSalt; // Allows access for reading and writing Path of the logbook
      property PassExportHash : String read FPassExportHash write FPassExportHash; // Allows access for reading and writing Path of the logbook
      property PassPrintSalt : String read FPassPrintSalt write FPassPrintSalt; // Allows access for reading and writing Path of the logbook
      property PassPrintHash : String read FPassPrintHash write FPassPrintHash; // Allows access for reading and writing Path of the logbook
      property AllowCategories : Boolean read FAllowCategories write FAllowCategories; // Allows access for reading and writing Path of the logbook
      property AllowAddCategories : Boolean read FAllowAddCategories write FAllowAddCategories; // Allows access for reading and writing Path of the logbook
      property AllowLateEntries : Boolean read FAllowLateEntries write FAllowLateEntries; // Allows access for reading and writing Path of the logbook
      property Categories: String read FCategories write FCategories; // Allows access for reading and writing Path of the logbook
      property DTDisplayFormat: String read FDTDisplayFormat write FDTDisplayFormat; // Allows access for reading and writing Path of the logbook
      property isError : String read FisError write FisError;
    end;


  {
  headerMark and footerMark definition:
  TEXT:      >TimberLog<
  ASCII:     #062 #084 #105 #109 #098 #101 #114 #076 #111 #103 #060
  }


implementation

{ TLogbook }

uses
  dmUnitDBTools;

// General class constructor
constructor TLogbook.Create;
begin
  isError := ''; // Initially, there is no fault, so the string is set to blank.
                 // But it may be set true while attempting to create the logbook
                 // in which case an error message will be set.
end;

// Overloaded constructor. For possible future use
constructor TLogbook.Create(Args: array of Integer);
begin

end;

destructor TLogbook.Destroy;
begin
  inherited Destroy;
end;

// Opens an existing .logb file
procedure TLogbook.openLogbook;
begin

  // The path should already be set prior to opening the logbook. Check to ensure it is!

  // Read the record in to FLogMetadata
  if ReadLogMetadata() = true then
  begin

    // Create Logbook file backup (and the new log file sans the RLogMetadata)
    CreateBackup();

    // Start running queries on the database!

  end;



end;

// Creates a new logbook database
procedure TLogbook.newLogbook();
begin



end;

// Used just before the formLogbook form closes, this procedure saves the record to the end of the logbook
// and calls DeteteBackup
procedure TLogbook.closeLogbook();
begin

  // Write the record to the end of the logbook
  WriteLogMetadata();

  // Remove the backup file
  DeleteBackupLog();


end;

// Reads the RLogMetadata record from a file, saves it to the class's LogMetadata
// And returns true or false depending on if it was successful

// Return of true means the LogMetadata exists and its format is valid
function TLogbook.readLogMetadata(tempRecord : Boolean): Boolean;
var
  FSRecord: TFileStream;
  tempLogMetadata : RLogMetadata;
begin
  readLogMetadata := False;

  try
    FSRecord := TFileStream.Create(path, fmOpenRead or fmShareDenyNone);
      try
        FSRecord.Seek(FSRecord.Size - SizeOf(RLogMetadata), soBeginning);  // Move to beginning of the record in the file
        FSRecord.Read(tempLogMetadata,sizeof(RLogMetadata)); // Read the record
        if verifyLogMetadataStructure(tempLogMetadata) then
        begin
          readLogMetadata := True;
          if tempRecord = False then
          begin
            LogMetadata := tempLogMetadata;
          end;
        end;

      except
        isError:='Error reading file during readLogMetadata';
      end;
  finally
    FSRecord.Free; // Save new file to disk and free stream
  end;

  {
  if SettingsValid = False then
  begin
    OpenedBy := 'The Logbook Is Corrupt or does not exist';
    footerMark := '00000000000';
  end
  else
  begin
    //
  end;
  }



end;

procedure TLogbook.buildLogMetadataRecord(AlogName, AlogDescription, AOpenedBy : String; ADTOpened, ADTAccessed : LongInt);
var
  str64 : String;
begin
  headerMark := RecHeader;
  footerMark := RecFooter;

  logName := AlogName;
  logDescription := AlogDescription;
  OpenedBy := AOpenedBy;
  DTOpened := ADTOpened;
  DTAccessed := ADTAccessed;

  dmCrypt.stringhash(AlogName + AlogDescription + AOpenedBy + IntToStr(ADTOpened) + IntToStr(ADTAccessed), str64);
  SetLength(str64, 64);
  checksum := str64;
  fileChecksum := '';

  CopyDataToLogMetadata();
end;

// Takes a file input and adds RLogMetadata to the end of the file
procedure TLogbook.writeLogMetadata();
var
  FSUpdate : TFileStream;
  tempString : String;
begin

  // checksum of file
  dmCrypt.filehash(Path, tempString);
  fileChecksum := tempString;

  // Only time we write to LogMetadata;
  CopyDataToLogMetadata;


  // Before we write to the logbook, perform a read
  // If it's successful, then the RLogMetadata entry already exists.
  // We'll overwrite it instead of appending to the end
  if ReadLogMetadata = false then
  begin
    //isError := 'Does not exist yet';
    if FileExists(Path) then
    begin

      try
        FSUpdate := TFileStream.Create(Path, fmOpenWrite or fmShareDenyNone);

          try
            FSUpdate.Seek(FSUpdate.Size, soBeginning);  // Move to end of output file
            FSUpdate.Write(LogMetadata, SizeOf(LogMetadata)); // Write the record to the file
          finally
            FSUpdate.Free; // Save new file to disk and free stream
          end;
      finally
      end;
    end;
  end
  else // if the record already exists...
  begin
    //isError := 'Does exist';
    if FileExists(Path) then
    begin
      try
        FSUpdate := TFileStream.Create(Path, fmOpenWrite or fmShareDenyNone);
        if FSUpdate.Size > SizeOf(LogMetadata)then // Don't try to overwrite if file is too small for some reason
        begin
          try
            FSUpdate.Seek(FSUpdate.Size - SizeOf(LogMetadata), soBeginning);  // Move to end of output file minus the record
            FSUpdate.Write(LogMetadata, SizeOf(LogMetadata)); // Write the record to the file
          finally
            FSUpdate.Free; // Save new file to disk and free stream
          end;
        end;

      finally
      end;
    end;
  end;

  // Clean up old logbook
  deleteBackupLog();

end;

// Deletes the old logbook file
procedure TLogbook.deleteBackupLog();
begin
  if FileExists(ChangeFileExt(Path, '.logbak')) then
  begin
    try
      DeleteFile(ChangeFileExt(Path, '.logbak'));
    finally
    end;
  end;
end;

procedure TLogbook.copyDataToLogMetadata;
begin
  LogMetadata.headerMark := RecHeader;
  LogMetadata.checksum := checksum;
  LogMetadata.fileChecksum := fileChecksum;
  LogMetadata.logName := logName;
  LogMetadata.logDescription := logDescription;
  LogMetadata.OpenedBy := OpenedBy;
  LogMetadata.DTOpened := DTOpened;
  LogMetadata.DTAccessed := DTAccessed;
  LogMetadata.PassMasterSalt := PassMasterSalt;
  LogMetadata.footerMark := RecFooter;
end;

procedure TLogbook.copyDataFromLogMetadata;
begin
  headerMark := LogMetadata.headerMark;
  checksum := LogMetadata.checksum;
  fileChecksum := LogMetadata.fileChecksum;
  logName := LogMetadata.logName;
  logDescription := LogMetadata.logDescription;
  OpenedBy := LogMetadata.OpenedBy;
  DTOpened := LogMetadata.DTOpened;
  DTAccessed := LogMetadata.DTAccessed;
  PassMasterSalt := LogMetadata.PassMasterSalt;
  footerMark := LogMetadata.footerMark;
end;

// Creates a copy of the original file sans the record, and saves
// the original file as a backip
procedure TLogbook.createBackup();
var
  FSInput, FSOutput, FSRecord: TFileStream;
  tiStart, tiEnd : TTime;
  BytesRead : Int64;
begin

  FSInput := TFileStream.Create(Path, fmOpenRead);
  try
    FSInput.Position := 0;  // Set to whatever starting position for the file being copied
    FSOutput := TFileStream.Create(ChangeFileExt(Path, '.new'), fmOpenWrite or fmCreate);  // Create output file (will become our new logbook)
    try
      FSOutput.CopyFrom(FSInput, FSInput.Size - SizeOf(LogMetadata));  // Copy bytes from input, except for the record
    finally
      FSOutput.Free;  // Save new file to disk and free stream
    end;
  finally
    FSInput.Free;  // Free input stream
  end;

  try
    RenameFile(ChangeFileExt(Path, '.logb'), ChangeFileExt(Path, '.logbak'));
    RenameFile(ChangeFileExt(Path, '.new'), ChangeFileExt(Path, '.logb'));
  finally
  end;

end;

procedure TLogbook.writeMetadataToMemo(outMemo: TMemo);
var
  allValid : Boolean;
begin
  allValid:=True;

  outMemo.Lines.Clear;

  outMemo.Lines.Add('Logbook Opened By: ' + Trim(OpenedBy));
  outMemo.Lines.Add('Logbook Serial Number/Name: ' + Trim(logName));
  try
    outMemo.Lines.Add('Logbook Opened On: ' + DateTimeToStr(FileDateToDateTime(DTOpened)));
    outMemo.Lines.Add('Logbook Last Accessed: ' + DateTimeToStr(FileDateToDateTime(DTAccessed)));
  except
    allValid := False;
  end;

  if verifyLogMetadataStructure(LogMetadata) then
  begin
    outMemo.Lines.Add('Checksum valid');
  end
  else
  begin
    allValid := False;
  end;

  outMemo.Lines.Add('Logbook Description: ' + Trim(logDescription));

  if Path <> 'Null' then
  begin
    outMemo.Lines.Add('Full Path to Logbook: ' + Path);
  end;

  { /// THIS NEEDS TO BE IMPLEMENTED AFTER TESTING IS COMPLETE
  ////////////////////////////////////////////////////////////////////////////////////
  if allValid = False then
  begin
    outMemo.Lines.Clear;
    outMemo.Lines.Add('The selected logbook is either corrupted or does not exist');
  end;
  }
end;

// Verifies that the header and footer of the record is valid
// Probability of these values being correct by mere chance are astronomical
function TLogbook.verifyLogMetadataStructure(tempLogMetadata : RLogMetadata) : Boolean;
begin

  if (tempLogMetadata.footerMark = RecFooter) and (tempLogMetadata.headerMark = RecHeader) then
  begin
    verifyLogMetadataStructure := True;
    //isError := 'true';
  end
  else
  begin
    verifyLogMetadataStructure := False;
    //isError := 'false';
  end;
end;

// Verifies that the header and footer of the record is valid
// Probability of these values being correct by mere chance are astronomical
function TLogbook.verifyLogMetadataContent(tempLogMetadata : RLogMetadata) : Boolean;
var
  str64 : String;
begin
  dmCrypt.stringhash(tempLogMetadata.logName + tempLogMetadata.logDescription + tempLogMetadata.OpenedBy + IntToStr(tempLogMetadata.DTOpened) + IntToStr(tempLogMetadata.DTAccessed), str64);
  SetLength(str64, 64);

  if (tempLogMetadata.footerMark = RecFooter) and (tempLogMetadata.headerMark = RecHeader) and (tempLogMetadata.checksum = str64) then
  begin
    verifyLogMetadataContent := True;
  end
  else
  begin
    verifyLogMetadataContent := False;
  end;
end;


end.

