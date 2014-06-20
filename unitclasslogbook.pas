unit unitClassLogbook;

// Class Unit for class Logbook

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitRecordLogMetadata, unitRecordLogMetadataExt;

type

  { TLogbook }

  TLogbook = class(TObject)
    const
      RecHeader = '>TimberLog<';
      RecFooter = '>TimberLog<';

    private // self access only
      FLogMetadataExt: RLogMetadataExt; // The Logbook Metadata Extended record
      FLogMetadata: RLogMetadata; // The Logbook Metadata record
      Path : String; 			// Full path to the logbook file (*.logb)
      logName : String[64]; 		// Serial Number or Name of equipment
      logDescription : String; 		// Short description or directions for this logbook
      OpenedBy : String[64]; 		// Name of person who originally opened the logbook
      checksum : String[64]; 		// Checksum of the metadata settings
      checksumsum : String[64]; 	// Checksum of entry checksums (This may just take too long to be reasonable)
      AllowCategories : Integer; 	// Are categories allowed in this logbook?
      AllowAddCategories : Integer; 	// Can users add to the list of categories (or are the categories constants?)
      AllowLateEntries : Integer; 	// Are users allowed to add late entries? (the logbook creator that the actual date is also saved)
      DTOpened : LongInt; 		// DateTime that the file was originally opened
      DTAccessed : LongInt; 		// DateTime that the file was last saved
      PassPerUser : Integer; 		// Are users required to use individual passwords to submit entries?
      PassToExport : Integer; 		// Are users required to enter a password to export to text/pdf/other formats?
      PassToPrint : Integer; 		// Are users required to enter a password to print the logbook?
      DTDisplayFormat : String; 	// Format string for the DateTime fields (internally stored as LongInt)

      function verifyLogMetadata:Boolean; // Verifies that FLogMetadata header and footer are valid

      // Make CreateBackup a function with boolean return
      procedure CreateBackup();            // Copies 'filepath', minus the record, and renames both files
      function ReadLogMetadata(): Boolean; // Reads the record from the end of 'filepath'
      procedure WriteLogMetadata();        // Writes FLogMetadata to file
      procedure DeleteBackupLog();         // Detes the backup logbook

    public // access by anything
      property LogMetadata: RLogMetadata read FLogMetadata write FLogMetadata; // Allows access for reading and writing FLogMetadata
      constructor Create; overload;
      constructor Create(Args: array of Integer); overload;
      destructor Destroy; override;

      // Make OpenLogbook a function with boolean return
      procedure OpenLogbook(filepath : String); // Opens an existing logbook file

      // Make NewLogbook a function with boolean return
      procedure NewLogbook(filepath : String; NewLogMetadataExt: RLogMetadataExt); // Creates a new logbook file database
      procedure CloseLogbook();            // Process for final logbook closing

    published // special type of public scope

    end;

implementation

{ TLogbook }



// General class constructor
constructor TLogbook.Create;
begin

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
procedure TLogbook.OpenLogbook(filepath: String);
begin

  // Save the logbook filepath in the path variable (used when closing the logbook)
  Path := filepath;

  // Read the record in to FLogMetadata
  if ReadLogMetadata() = true then
  begin

    // Create Logbook file backup (and the new log file sans the RLogMetadata)
    CreateBackup();

    // Start running queries on the database!

  end;



end;

// Creates a new logbook database
// RLogMetadata is passed to FLogMetadata
procedure TLogbook.NewLogbook(filepath: String; NewLogMetadataExt: RLogMetadataExt);
begin

  // Save the logbook filepath in the path variable (used when closing the logbook)
  Path := filepath;

  // Take the input RLogMetadata and aapply it to the class's FLogMetadata;
  FLogMetadataExt := NewLogMetadataExt;



end;

// Used just before the formLogbook form closes, this procedure saves the record to the end of the logbook
// and calls DeteteBackup
procedure TLogbook.CloseLogbook();
begin

  // Write the record to the end of the logbook
  WriteLogMetadata();

  // Remove the backup file
  DeleteBackupLog();


end;

// Reads the RLogMetadata record from a file, saves it to the class's FLogMetadata
// And returns tru or false depending on if it was successful
function TLogbook.ReadLogMetadata(): Boolean;
var
  SettingsValid : Boolean;
  BytesRead : Int64;
  FSRecord: TFileStream;
begin
  SettingsValid := False;

  if FileExists(Path) then
  begin

      try
        FSRecord := TFileStream.Create(Path, fmOpenRead);

        // Verify that the file size is large enough for the record to be present
        if FSRecord.Size >= SizeOf(FLogMetadata) then
        begin
          try
            FSRecord.Seek(FSRecord.Size - SizeOf(FLogMetadata), soBeginning);  // Move to beginning of the record in the file
            BytesRead := FSRecord.Read(FLogMetadata,sizeof(FLogMetadata)); // Read the record

            if verifyLogMetadata = True then
            begin
              SettingsValid := True;
            end;

          finally

          end;
        end
        else
        begin
          //
        end;
      finally
        FSRecord.Free; // Save new file to disk and free stream
      end;

  end;

    if SettingsValid = False then
    begin
      ReadLogMetadata := False;
    end
    else
    begin
      ReadLogMetadata := True;
    end;


end;

// Takes a file input and adds RLogMetadata to the end of the file
procedure TLogbook.WriteLogMetadata();
var
  FSUpdate : TFileStream;
begin

  // Before we write to the logbook, perform a read
  // If it's successful, then the RLogMetadata entry already exists.
  // We'll overwrite it instead of appending to the end
  if ReadLogMetadata() = false then
  begin

    if FileExists(Path) then
    begin





      //////  NEED TO: Obtain checksum of file                 \\\\\\
      //////  NEED TO: FLogMetadata.checksum := fileChecksum;  \\\\\\





      try
        FSUpdate := TFileStream.Create(Path, fmOpenWrite);
          try
            FSUpdate.Seek(FSUpdate.Size, soBeginning);  // Move to end of output file
            FSUpdate.Write(FLogMetadata, SizeOf(FLogMetadata)); // Write the record to the file
          finally
            FSUpdate.Free; // Save new file to disk and free stream
          end;
      finally
      end;
    end;
  end
  else // if the record already exists...
  begin
    if FileExists(Path) then
    begin
      try
        FSUpdate := TFileStream.Create(Path, fmOpenWrite);
        if FSUpdate.Size > SizeOf(FLogMetadata)then // Don't try to overwrite if file is too small for some reason
        begin
          try
            FSUpdate.Seek(FSUpdate.Size - SizeOf(FLogMetadata), soBeginning);  // Move to end of output file minus the record
            FSUpdate.Write(FLogMetadata, SizeOf(FLogMetadata)); // Write the record to the file
          finally
            FSUpdate.Free; // Save new file to disk and free stream
          end;
        end;

      finally
      end;
    end;
  end;



  // Clean up old logbook
  DeleteBackupLog();

end;

// Deletes the old logbook file
procedure TLogbook.DeleteBackupLog();
begin
  if FileExists(ChangeFileExt(Path, '.logbak')) then
  begin
    try
      DeleteFile(ChangeFileExt(Path, '.logbak'));
    finally
    end;
  end;
end;

// Creates a copy of the original file sans the record, and saves
// the original file as a backip
procedure TLogbook.CreateBackup();
var
  MLogMetadata : RLogMetadata;
  FSInput, FSOutput, FSRecord: TFileStream;
  tiStart, tiEnd : TTime;
  BytesRead : Int64;
begin

  FSInput := TFileStream.Create(Path, fmOpenRead);
  try
    FSInput.Position := 0;  // Set to whatever starting position for the file being copied
    FSOutput := TFileStream.Create(ChangeFileExt(Path, '.new'), fmOpenWrite or fmCreate);  // Create output file (will become our new logbook)
    try
      FSOutput.CopyFrom(FSInput, FSInput.Size - SizeOf(MLogMetadata));  // Copy bytes from input, except for the record
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

// Verifies that the header and footer of the record is valid
// Probability of these values being correct by mere chance are astronomical
function TLogbook.verifyLogMetadata: Boolean;
begin
  if (LogMetadata.headerMark = RecHeader) and (LogMetadata.footerMark = RecFooter) then
  begin
    verifyLogMetadata := True;
  end
  else
  begin
    verifyLogMetadata := False;
  end;
end;


end.

