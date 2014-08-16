unit unitRecordLogMetadata;

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
    HeaderMark : array [0..10] of char;		// Used to verify TLogMetadata
    Checksum : array [0..63] of char;		// Checksum of RLogMetadata
    FileChecksum : array [0..63] of char;	// Checksum of entire file (minus RLogMetadata)
    LogName : array [0..63] of char;		// Serial Number or Name of equipment (Should match the value in Settings table)
    LogDescription : array [0..1023] of char;	// Short description or directions for this logbook (Should match the value in Settings table)
    OpenedBy : array [0..63] of char;		// Name of person opening the logbook (Should match the value in Settings table)
    DTOpened : LongInt;				// DateTime that the file was originally opened (Should match the value in Settings table)
    DTAccessed : LongInt;			// DateTime that the file was last saved (Should match the value in Settings table)
    PassMasterSalt : array [0..63] of char;//String[64];                // Salt for the Master Password
    FooterMark : array [0..10] of char;		// Used to verify TLogMetadata
  end;


  {
  HeaderMark and FooterMark definition:
  TEXT:      >TimberLog<
  ASCII:     #062 #084 #105 #109 #098 #101 #114 #076 #111 #103 #060
  }

implementation

end.

