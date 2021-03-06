unit unitClassLogbook;

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

// Class Unit for class Logbook

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitRecordLogMetadata, unitDefinitions, dmUnitCrypt, StdCtrls, LazLogger;

type

  { TLogbook }

  TLogbook = class(TObject)
    private // self access only
      FHeaderMark : String;       // used to verify TLogMetadata
      FFooterMark : String;       // used to verify TLogMetadata
      FPath : String; 			// Full path to the logbook file (*.logb)
      FLogName : String;		// Serial Number or Name of equipment (Should match the value in Settings table)
      FLogDescription : String;		// Short description or directions for this logbook (Should match the value in Settings table)
      FOpenedBy : String;		// Name of person opening the logbook (Should match the value in Settings table)
      FChecksum : String; 		// Checksum of the metadata settings
      FFileChecksum : String;           // Checksum of the entire File minus RLogMetadata)
      FChecksumsum : String; 	        // Checksum of all entry Checksums (This may just take too long to be reasonable)
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
      FDTDisplayFormat : String; 	 // Format String for the DateTime fields (internally stored as LongInt)
      FIsError : String;                 // Tests whether the logbook is in a fault condition at any point during its creation

    public // access by anything
      LogMetadata: RLogMetadata;        // The Logbook Metadata record.
                                        // Its purpose is only for reading/writing metadata from/to the logbook file

      procedure WriteMetadataToMemo(outMemo : TMemo);
      function VerifyLogMetadataStructure(tempLogMetadata : RLogMetadata):Boolean ; // Verifies that FLogMetadata header and footer are valid
      function VerifyLogMetadataContent(tempLogMetadata : RLogMetadata):Boolean ; // Verifies that FLogMetadata header and footer are valid
      procedure CreateBackup;            // Copies 'filepath', minus the record, and renames both files
      function ReadLogMetadata(tempRecord : Boolean = False): Boolean; // Reads the record from the end of 'filepath'
      procedure BuildLogMetadataRecord(ALogName, ALogDescription, AOpenedBy : String; ADTOpened, ADTAccessed : LongInt); // Processes data from Page 1 and creates RLogMetadata record
      procedure WriteLogMetadata;        // Writes FLogMetadata to file
      procedure DeleteBackupLog;         // Deletes the backup logbook
      procedure CopyDataToLogMetadata;         // Copies the data from the main class parameters to LogMetadata
      procedure CopyDataFromLogMetadata;         // Copies the data from LogMetadata to the main class parameters

      // Opens the database via filestream and manually checks/returns the user_version and application_id pragma values
      procedure CheckPragmaManually({var BUserVersion : LongInt; }var BApplicationID : Cardinal);

      constructor Create; overload;
      constructor Create(Args: array of Integer); overload;
      destructor Destroy; override;

      // Make OpenLogbook a function with boolean return
      procedure OpenLogbook; // Opens an existing logbook file

      // Make NewLogbook a function with boolean return
      procedure NewLogbook; // Creates a new logbook file database
      procedure CloseLogbook;            // Process for final logbook closing

    published // special type of public scope
      property Path: String read FPath write FPath; // Allows access for reading and writing Path of the logbook
      property HeaderMark: String read FHeaderMark write FHeaderMark; // Allows access for reading and writing Path of the logbook
      property FooterMark: String read FFooterMark write FFooterMark; // Allows access for reading and writing Path of the logbook
      property LogName: String read FLogName write FLogName; // Allows access for reading and writing Path of the logbook
      property LogDescription: String read FLogDescription write FLogDescription; // Allows access for reading and writing Path of the logbook
      property OpenedBy: String read FOpenedBy write FOpenedBy; // Allows access for reading and writing Path of the logbook
      property Checksum: String read FChecksum write FChecksum; // Allows access for reading and writing Path of the logbook
      property FileChecksum: String read FFileChecksum write FFileChecksum; // Allows access for reading and writing Path of the logbook
      property Checksumsum: String read FChecksumsum write FChecksumsum; // Allows access for reading and writing Path of the logbook
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
      property IsError : String read FIsError write FIsError;
    end;

  {
  HeaderMark and FooterMark definition:
  TEXT:      >TimberLog<
  ASCII:     #062 #084 #105 #109 #098 #101 #114 #076 #111 #103 #060
  }

  RSQLiteHeader = packed record
    AStart : array [0..59] of byte;
    AUserVersion : LongInt;
    AVaccuum : array [0..3] of byte;
    AApplicationID : Cardinal;
  end;


implementation

{ TLogbook }

{uses
  dmUnitDBTools;}

// General class constructor
constructor TLogbook.Create;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.Create'); {$endif}

  IsError := ''; // Initially, there is no fault, so the String is set to blank.
                 // But it may be set True while attempting to create the logbook
                 // in which case an error message will be set.
end;

// Overloaded constructor. For possible future use
constructor TLogbook.Create(Args: array of Integer);
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.Create(Args: array of Integer)'); {$endif}

end;

destructor TLogbook.Destroy;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.Destroy'); {$endif}
  inherited Destroy;
end;

// Opens an existing .logb file
procedure TLogbook.OpenLogbook;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.OpenLogbook'); {$endif}

  // The path should already be set prior to opening the logbook. Check to ensure it is!

  // Read the record in to FLogMetadata
  if ReadLogMetadata() = True then
  begin

    // Create Logbook file backup (and the new log file sans the RLogMetadata)
    CreateBackup();

    // ToDo: Start running queries on the database!

  end;



end;

// Creates a new logbook database
procedure TLogbook.NewLogbook();
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.NewLogbook'); {$endif}



end;

// Used just before the formLogbook form closes, this procedure saves the record to the end of the logbook
// and calls DeteteBackup
procedure TLogbook.CloseLogbook();
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.CloseLogbook'); {$endif}

  // Write the record to the end of the logbook
  WriteLogMetadata();

  // Remove the backup file
  DeleteBackupLog();


end;

// Reads the RLogMetadata record from a file, saves it to the class's LogMetadata
// And returns True or False depending on if it was successful

// Return of True means the LogMetadata exists and its format is valid
function TLogbook.ReadLogMetadata(TempRecord : Boolean): Boolean;
var
  FSRecord: TFileStream;
  TempLogMetadata : RLogMetadata;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.ReadLogMetadata'); {$endif}

  ReadLogMetadata := False;

  try
    FSRecord := TFileStream.Create(path, fmOpenRead or fmShareDenyNone);
      try
        FSRecord.Seek(FSRecord.Size - SizeOf(RLogMetadata), soBeginning);  // Move to beginning of the record in the file
        FSRecord.Read(TempLogMetadata,sizeof(RLogMetadata)); // Read the record
        if VerifyLogMetadataStructure(tempLogMetadata) then
        begin
          ReadLogMetadata := True;
          if TempRecord = False then
          begin
            LogMetadata := TempLogMetadata;
          end;
        end;

      except
        IsError:='Error reading file during readLogMetadata';
      end;
  finally
    FSRecord.Free; // Save new file to disk and free stream
  end;

  {
  if SettingsValid = False then
  begin
    OpenedBy := 'The Logbook Is Corrupt or does not exist';
    FooterMark := '00000000000';
  end
  else
  begin
    //
  end;
  }



end;

procedure TLogbook.BuildLogMetadataRecord(ALogName, ALogDescription, AOpenedBy : String; ADTOpened, ADTAccessed : LongInt);
var
  str64 : String;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.BuildLogMetadataRecord'); {$endif}

  HeaderMark := RecHeader;
  FooterMark := RecFooter;

  LogName := ALogName;
  LogDescription := ALogDescription;
  OpenedBy := AOpenedBy;
  DTOpened := ADTOpened;
  DTAccessed := ADTAccessed;

  dmCrypt.stringhash(ALogName + ALogDescription + AOpenedBy + IntToStr(ADTOpened) + IntToStr(ADTAccessed), str64);
  SetLength(str64, 64);
  Checksum := str64;
  FileChecksum := '';

  CopyDataToLogMetadata();
end;

// Takes a file input and adds RLogMetadata to the end of the file
procedure TLogbook.WriteLogMetadata;
var
  FSUpdate : TFileStream;
  TempString : String;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.WriteLogMetadata'); {$endif}

  // Checksum of file
  dmCrypt.Filehash(Path, TempString);
  FileChecksum := TempString;

  // Only time we write to LogMetadata;
  CopyDataToLogMetadata;


  // Before we write to the logbook, perform a read
  // If it's successful, then the RLogMetadata entry already exists.
  // We'll overwrite it instead of appending to the end
  if ReadLogMetadata = False then
  begin
    //IsError := 'Does not exist yet';
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
    //IsError := 'Does exist';
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
  DeleteBackupLog();

end;

// Deletes the old logbook file
procedure TLogbook.DeleteBackupLog();
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.DeleteBackupLog'); {$endif}

  if FileExists(ChangeFileExt(Path, '.logbak')) then
  begin
    try
      DeleteFile(ChangeFileExt(Path, '.logbak'));
    finally
    end;
  end;
end;

procedure TLogbook.CopyDataToLogMetadata;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.CopyDataToLogMetadata'); {$endif}

  LogMetadata.HeaderMark := RecHeader;
  LogMetadata.Checksum := Checksum;
  LogMetadata.FileChecksum := FileChecksum;
  LogMetadata.LogName := LogName;
  LogMetadata.LogDescription := LogDescription;
  LogMetadata.OpenedBy := OpenedBy;
  LogMetadata.DTOpened := DTOpened;
  LogMetadata.DTAccessed := DTAccessed;
  LogMetadata.PassMasterSalt := PassMasterSalt;
  LogMetadata.FooterMark := RecFooter;
end;

procedure TLogbook.CopyDataFromLogMetadata;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.CopyDataFromLogMetadata'); {$endif}

  HeaderMark := LogMetadata.HeaderMark;
  Checksum := LogMetadata.Checksum;
  FileChecksum := LogMetadata.FileChecksum;
  LogName := LogMetadata.LogName;
  LogDescription := LogMetadata.LogDescription;
  OpenedBy := LogMetadata.OpenedBy;
  DTOpened := LogMetadata.DTOpened;
  DTAccessed := LogMetadata.DTAccessed;
  PassMasterSalt := LogMetadata.PassMasterSalt;
  FooterMark := LogMetadata.FooterMark;
end;

// This procedure is used to help determine whether a logbook file is encrypted or not
// Really, we only use the application_id value, but user_version may be useful
// to have at some later point
// If Bapplication_id doesn't match one of our valid id's, the file is likely encrypted,
// and we should ask the user for a password and try to open the Logbook that way.
procedure TLogbook.CheckPragmaManually({var BUserVersion: LongInt; }
  var BApplicationID: Cardinal);
var
  FSRecord: TFileStream;
  ASQLiteHeader : RSQLiteHeader;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.CheckPragmaManually'); {$endif}

  try
    FSRecord := TFileStream.Create('./new.db', fmOpenRead or fmShareDenyNone);
      try
        FSRecord.Seek(0, soBeginning);  // Move to beginning of the record in the file
        FSRecord.Read(ASQLiteHeader, SizeOf(RSQLiteHeader)); // Read the record

      except
        //IsError := '';
      end;
  finally
    FSRecord.Free; // Save new file to disk and free stream
  end;

  BApplicationID := SwapEndian(ASQLiteHeader.AApplicationID);
  {BUserVersion := SwapEndian(ASQLiteHeader.AUserVersion);}

end;

// Creates a copy of the original file sans the record, and saves
// the original file as a backip
procedure TLogbook.CreateBackup;
var
  FSInput, FSOutput, FSRecord: TFileStream;
  tiStart, tiEnd : TTime;
  BytesRead : Int64;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.CreateBackup'); {$endif}

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

procedure TLogbook.WriteMetadataToMemo(OutMemo: TMemo);
var
  AllValid : Boolean;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.WriteMetadataToMemo'); {$endif}

  AllValid:=True;

  OutMemo.Lines.Clear;

  OutMemo.Lines.Add('Logbook Opened By: ' + Trim(OpenedBy));
  OutMemo.Lines.Add('Logbook Serial Number/Name: ' + Trim(LogName));

  try
    OutMemo.Lines.Add('Logbook Opened On: ' + DateTimeToStr(FileDateToDateTime(DTOpened)));
    OutMemo.Lines.Add('Logbook Last Accessed: ' + DateTimeToStr(FileDateToDateTime(DTAccessed)));
  except
    AllValid := False;
  end;

  if VerifyLogMetadataStructure(LogMetadata) then
  begin
    OutMemo.Lines.Add('Checksum valid');
  end
  else
  begin
    AllValid := False;
  end;

  OutMemo.Lines.Add('Logbook Description: ' + Trim(LogDescription));

  if Path <> 'Null' then
  begin
    OutMemo.Lines.Add('Full Path to Logbook: ' + Path);
  end;

  { /// ToDo: THIS NEEDS TO BE IMPLEMENTED AFTER TESTING IS COMPLETE
  ////////////////////////////////////////////////////////////////////////////////////
  if AllValid = False then
  begin
    OutMemo.Lines.Clear;
    OutMemo.Lines.Add('The selected logbook is either corrupted or does not exist');
  end;
  }
end;

// Verifies that the header and footer of the record is valid
// Probability of these values being correct by mere chance are astronomical
function TLogbook.VerifyLogMetadataStructure(TempLogMetadata : RLogMetadata) : Boolean;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.VerifyLogMetadataStructure'); {$endif}

  if (TempLogMetadata.FooterMark = RecFooter) and (TempLogMetadata.HeaderMark = RecHeader) then
  begin
    VerifyLogMetadataStructure := True;
    //IsError := 'True';
  end
  else
  begin
    VerifyLogMetadataStructure := False;
    //IsError := 'False';
  end;
end;

// Verifies that the header and footer of the record is valid
// Probability of these values being correct by mere chance are astronomical
function TLogbook.VerifyLogMetadataContent(TempLogMetadata : RLogMetadata) : Boolean;
var
  str64 : String;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.verifyLogMetadataContent'); {$endif}
  dmCrypt.stringhash(TempLogMetadata.LogName + TempLogMetadata.LogDescription + TempLogMetadata.OpenedBy + IntToStr(TempLogMetadata.DTOpened) + IntToStr(TempLogMetadata.DTAccessed), str64);
  SetLength(str64, 64);

  if (TempLogMetadata.FooterMark = RecFooter) and (TempLogMetadata.HeaderMark = RecHeader) and (TempLogMetadata.Checksum = str64) then
  begin
    VerifyLogMetadataContent := True;
  end
  else
  begin
    VerifyLogMetadataContent := False;
  end;
end;


end.

