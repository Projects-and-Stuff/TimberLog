unit unitRecordLogMetadata;

// Record Unit for record LogMetadata

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  // The TLogMetadata record gets its own unit
  // This is the packed record appended to the logbook file
  // It provides basic metadata about the logbook for display to end users
  RLogMetadata = packed record
    headerMark : array [0..10] of char;		// Used to verify TLogMetadata
    checksum : array [0..63] of char;		// Checksum of RLogMetadata
    fileChecksum : array [0..63] of char;	// Checksum of entire file (minus RLogMetadata)
    logName : array [0..63] of char;		// Serial Number or Name of equipment (Should match the value in Settings table)
    logDescription : array [0..1023] of char;	// Short description or directions for this logbook (Should match the value in Settings table)
    OpenedBy : array [0..63] of char;		// Name of person opening the logbook (Should match the value in Settings table)
    DTOpened : LongInt;				// DateTime that the file was originally opened (Should match the value in Settings table)
    DTAccessed : LongInt;			// DateTime that the file was last saved (Should match the value in Settings table)
    PassMasterSalt : array [0..63] of char;//String[64];                // Salt for the Master Password
    footerMark : array [0..10] of char;		// Used to verify TLogMetadata
  end;


  {
  headerMark and footerMark definition:
  TEXT:      >TimberLog<
  ASCII:     #062 #084 #105 #109 #098 #101 #114 #076 #111 #103 #060
  }

implementation

end.

