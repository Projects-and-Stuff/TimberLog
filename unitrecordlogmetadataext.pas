unit unitRecordLogMetadataExt;

// Record Unit for record LogMetadataExt
// Used to pass the data for a new logbook from formStartDialog to formLogbook

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

    // The TLogMetadataExt record gets its own unit
    // Used only to pass the data for a new logbook from formStartDialog to formLogbook
    RLogMetadataExt = record
      logName : String;			// Serial Number or Name of equipment (Should match the value in Settings table)
      logDescription : String;		// Short description or directions for this logbook (Should match the value in Settings table)
      OpenedBy : String;		// Name of person opening the logbook (Should match the value in Settings table)
      DTOpened : LongInt;		// DateTime that the file was originally opened (Should match the value in Settings table)
      DTAccessed : LongInt;		// DateTime that the file was last saved (Should match the value in Settings table)
      PassMaster : Boolean;		// Is a master password required?
      PassPerUser : Boolean;		// Are users required to use individual passwords to submit entries?
      PassToExport : Boolean;		// Are users required to enter a password to export to text/pdf/other formats?
      PassToPrint : Boolean;		// Are users required to enter a password to print the logbook?
      PassMasterHash : String;           // Hashed Master Password
      PassExportHash : String;           // Hashed Export Password (Can be recovered using master password)
      PassPrintHash : String;            // Hashed Print Password  (Can be recovered using master password)
      AllowCategories : Boolean;	// Are categories allowed in this logbook?
      AllowAddCategories : Boolean;	// Can users add to the list of categories (or are the categories constants?)
      AllowLateEntries : Boolean;	// Are users allowed to add late entries? (the logbook creator that the actual date is also saved)
      Categories : TStringList;         // The list of categories
      DTDisplayFormat : String; 	// Format string for the DateTime fields (internally stored as LongInt)
    end;


  {
  headerMark and footerMark definition:
  TEXT:      >TimberLog<
  ASCII:     #062 #084 #105 #109 #098 #101 #114 #076 #111 #103 #060
  }

implementation

end.

