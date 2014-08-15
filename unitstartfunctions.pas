unit unitStartFunctions;

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

// Provides all the necessary functionality to allow
// formUnitStartDialog to focus on the GUI

// 1. Loads Logbook types and passes back to formUnitStartDialog
// 2. Reads Metadata record from any selected logbook and returns the record to formUnitStartDialog
// 3. When creating a new logbook, passes the filename and Metadata record to formLogbook
// 4. When opening a logbook, passes the filename to formLogbook

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, shlobj, {unitRecordLogMetadata,} strutils, unitDefinitions, {dmUnitCrypt,} Controls{, StdCtrls};

  procedure resetPages; // Resets Page 1 to the default values
  function returnDocumentsPath : String;
  procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type : String; Description : TStringList); // Short form used to identify the type name and description
  procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type : String; Description : TStringList;  // Short form used to identify the type name, description, and categories
    Default_Categories : TStringList);
  procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type : String; Description : TStringList; // Full form with entire type definition
    Settings: TStringList; Default_Categories: TStringList; Default_Text: TStringList;
    One_Line: TStringList; Multi_Line: TStringList);
  procedure getTypeFileList(var fileList : TStringList);
  procedure Split(const Delimiter : Char; Input : string; const Strings : TStrings);

implementation

uses
  formUnitStartDialog;

procedure Split(const Delimiter : Char; Input : string; const Strings : TStrings);
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.StrictDelimiter := true;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

procedure resetPages;
begin
  //formStartDialog.Notebook1.PageIndex := 0; // Reset to the default page on formStartDialog

  // Clear out all the components on page 1
  formStartDialog.txtLogName.Clear;
  formStartDialog.memoLogDescription.Clear;
  formStartDialog.chkPassMaster.Checked:=False;
  formStartDialog.chkPassToExport.Checked:=False;
  formStartDialog.chkPassToPrint.Checked:=False;
  formStartDialog.chkPassPerUser.Checked:=False;
  formStartDialog.txtMasterPass.Clear;
  formStartDialog.txtMasterPass1.Clear;
  formStartDialog.txtPassToExport.Clear;
  formStartDialog.txtPassToExport1.Clear;
  formStartDialog.txtPassToPrint.Clear;
  formStartDialog.txtPassToPrint1.Clear;
  formStartDialog.txtOpenedBy.Clear;
  formStartDialog.cmbDFormat.ItemIndex:=0;
  formStartDialog.cmbTFormat.ItemIndex:=0;
  formStartDialog.chkAllowCategories.Checked:=False;
  formStartDialog.chkAllowAddCategories.Checked:=False;
  formStartDialog.txtCategory.Clear;
  formStartDialog.listboxCategories.Clear;

  // Reset the path for ShellTreeView on page 2
  try
    if not DirectoryExists(returnDocumentsPath + '\TimberLog\') then  // Check if TimberLog user directory exists
    begin
      MkDir(returnDocumentsPath + '\TimberLog\');
    end;

    formStartDialog.shellTreeSelectFolder.Path := returnDocumentsPath + '\TimberLog\';
  finally

  end;

end;

function returnDocumentsPath: String;
var
  DocumentsPath: Array[0..MaxPathLen] of Char; //Allocate memory
begin
  DocumentsPath := '';
  SHGetSpecialFolderPath(0,DocumentsPath,CSIDL_PERSONAL,false); // http://delphi-miranda-plugins.googlecode.com/svn-history/r105/trunk/FPC/units/src/shlobj.pp
  returnDocumentsPath := DocumentsPath;
end;






///////////////////////////////////////////////////////////////////////////////////////////////////
// ToDo: Consider splitting these various actions into separate procedures for better modularity //
///////////////////////////////////////////////////////////////////////////////////////////////////

procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type: String; Description: TStringList);
var
  utilStrList : TStringList; // Holds the entire Type File while we process it

  // These loc arrays are used for identifying the type data
  // The first value (IE: locType[0]) counts how many instances there are so far
  // All additional values specify the location of the blocks
  locLogbook_Type, locDescription : array of Integer;

  i : Integer;
  hasError : Boolean; // Used to check whether there is an error in number of block elements
begin
  utilStrList := TStringList.Create;
  utilStrList.LoadFromFile(filepath); // Load up the type file into the utility TStringList

  // Set the initial length of locating arrays
  SetLength(locLogbook_Type, 1);
  SetLength(locDescription, 1);

  // Initially, there are no instances of each type block
  locLogbook_Type[0] := 0;
  locDescription[0] := 0;


  // Initially, run through the entire file looking for the total number of each block
  for i := 0 to utilStrList.Count-1 do
  begin

    if (AnsiContainsText(blockLogbook_Type, utilStrList.Strings[i]) = true) then
    begin
      locLogbook_Type[0] := locLogbook_Type[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(blockDescription, utilStrList.Strings[i]) = true) then
    begin
      locDescription[0] := locDescription[0] + 1; // If we've got a match, increase the count
    end;

    // Add further block element searches here...
  end;

  // Set the array lengths based upon the above
  SetLength(locLogbook_Type, locLogbook_Type[0]+1);
  SetLength(locDescription, locDescription[0]+1);

  // Reset the counters
  locLogbook_Type[0] := 0;
  locDescription[0] := 0;

  // The, run through the entire file again for each instance of each block
  for i := 0 to utilStrList.Count-1 do
  begin

    if (AnsiContainsText(blockLogbook_Type, utilStrList.Strings[i]) = true) then
    begin
      locLogbook_Type[0] := locLogbook_Type[0] + 1; // If we've got a match, increase the count
      locLogbook_Type[locLogbook_Type[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    if (AnsiContainsText(blockDescription, utilStrList.Strings[i]) = true) then
    begin
      locDescription[0] := locDescription[0] + 1; // If we've got a match, increase the count
      locDescription[locDescription[0]] := i; // Add the new value to the array showing the location of a block element
    end;

  // Add further block element searches here...
  end;


  // Check to ensure each block has an even number of elements
  hasError := False;
  errorMsg := '';

  if ((Length(locLogbook_Type)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockLogbook_Type + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;

  if ((Length(locDescription)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockDescription + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;



  // Then process the blocks into their respective variables (String or TStringList)
  if errorMsg = '' then // If there's no error, keep on processing
  begin

    // Logbook_Type is a special case since only one entry is allowed, and only one line is allowed
    Logbook_Type := utilStrList.Strings[locLogbook_Type[1]+1];

    // Add the description, which is a single multi-line block (additional intances of Description are ignored)
    for i := locDescription[1]+1 to locDescription[2]-1 do
    begin
      Description.AddText(utilStrList.Strings[i]);
    end;

  end;

  utilStrList.Free;
end;

procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type: String; Description: TStringList;
  Default_Categories: TStringList);
var
  utilStrList : TStringList; // Holds the entire Type File while we process it

  // These loc arrays are used for identifying the type data
  // The first value (IE: locType[0]) counts how many instances there are so far
  // All additional values specify the location of the blocks
  locLogbook_Type, locDescription, locDefault_Categories : array of Integer;

  i : Integer;
  hasError : Boolean; // Used to check whether there is an error in number of block elements
begin
  utilStrList := TStringList.Create;
  utilStrList.LoadFromFile(filepath); // Load up the type file into the utility TStringList

  // Set the initial length of locating arrays
  SetLength(locLogbook_Type, 1);
  SetLength(locDescription, 1);
  SetLength(locDefault_Categories, 1);

  // Initially, there are no instances of each type block
  locLogbook_Type[0] := 0;
  locDescription[0] := 0;
  locDefault_Categories[0] := 0;


  // Initially, run through the entire file looking for the total number of each block
  for i := 0 to utilStrList.Count-1 do
  begin

    if (AnsiContainsText(blockLogbook_Type, utilStrList.Strings[i]) = true) then
    begin
      locLogbook_Type[0] := locLogbook_Type[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(blockDescription, utilStrList.Strings[i]) = true) then
    begin
      locDescription[0] := locDescription[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(blockDefault_Categories, utilStrList.Strings[i]) = true) then
    begin
      locDefault_Categories[0] := locDefault_Categories[0] + 1; // If we've got a match, increase the count
    end;

    // Add further block element searches here...
  end;

  // Set the array lengths based upon the above
  SetLength(locLogbook_Type, locLogbook_Type[0]+1);
  SetLength(locDescription, locDescription[0]+1);
  SetLength(locDefault_Categories, locDefault_Categories[0]+1);

  // Reset the counters
  locLogbook_Type[0] := 0;
  locDescription[0] := 0;
  locDefault_Categories[0] := 0;

  // The, run through the entire file again for each instance of each block
  for i := 0 to utilStrList.Count-1 do
  begin

    if (AnsiContainsText(blockLogbook_Type, utilStrList.Strings[i]) = true) then
    begin
      locLogbook_Type[0] := locLogbook_Type[0] + 1; // If we've got a match, increase the count
      locLogbook_Type[locLogbook_Type[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    if (AnsiContainsText(blockDescription, utilStrList.Strings[i]) = true) then
    begin
      locDescription[0] := locDescription[0] + 1; // If we've got a match, increase the count
      locDescription[locDescription[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    if (AnsiContainsText(blockDefault_Categories, utilStrList.Strings[i]) = true) then
    begin
      locDefault_Categories[0] := locDefault_Categories[0] + 1; // If we've got a match, increase the count
      locDefault_Categories[locDefault_Categories[0]] := i; // Add the new value to the array showing the location of a block element
    end;

  // Add further block element searches here...
  end;


  // Check to ensure each block has an even number of elements
  hasError := False;
  errorMsg := '';

  if ((Length(locLogbook_Type)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockLogbook_Type + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;

  if ((Length(locDescription)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockDescription + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;

  if ((Length(locDefault_Categories)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockDefault_Categories + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;



  // Then process the blocks into their respective variables (String or TStringList)
  if errorMsg = '' then // If there's no error, keep on processing
  begin

    // Logbook_Type is a special case since only one entry is allowed, and only one line is allowed
    Logbook_Type := utilStrList.Strings[locLogbook_Type[1]+1];

    // Add the Description, which is a single multi-line block (additional intances of Description are ignored)
    for i := locDescription[1]+1 to locDescription[2]-1 do
    begin
      Description.AddText(utilStrList.Strings[i]);
    end;

    // Add the Default_Categories, which is a single multi-line block (additional intances of Default_Categories are ignored)
    for i := locDefault_Categories[1]+1 to locDefault_Categories[2]-1 do
    begin
      Default_Categories.AddText(utilStrList.Strings[i]);
    end;

  end;

  utilStrList.Free;
end;

procedure processTypeFile(filepath : String; out errorMsg : String; out Logbook_Type: String; Description: TStringList;
  Settings : TStringList; Default_Categories : TStringList; Default_Text: TStringList; One_Line: TStringList; Multi_Line: TStringList);
var
  utilStrList : TStringList; // Holds the entire Type File while we process it

  // These loc arrays are used for identifying the type data
  // The first value (IE: locType[0]) counts how many instances there are so far
  // All additional values specify the location of the blocks
  locLogbook_Type, locDescription, locSettings, locDefault_Categories, locDefault_Text, locOne_Line, locMulti_Line : array of Integer;

  i : Integer;
  hasError : Boolean; // Used to check whether there is an error in number of block elements
begin
  utilStrList := TStringList.Create;
  utilStrList.LoadFromFile(filepath); // Load up the type file into the utility TStringList

  // Set the initial length of locating arrays
  SetLength(locLogbook_Type, 1);
  SetLength(locDescription, 1);
  SetLength(locDefault_Categories, 1);
  SetLength(locSettings, 1);
  SetLength(locDefault_Text, 1);
  SetLength(locOne_Line, 1);

  // Initially, there are no instances of each type block
  locLogbook_Type[0] := 0;
  locDescription[0] := 0;
  locDefault_Categories[0] := 0;
  locSettings[0] := 0;
  locDefault_Text[0] := 0;
  locOne_Line[0] := 0;


  // Initially, run through the entire file looking for the total number of each block
  for i := 0 to utilStrList.Count-1 do
  begin

    if (AnsiContainsText(blockLogbook_Type, utilStrList.Strings[i]) = true) then
    begin
      locLogbook_Type[0] := locLogbook_Type[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(blockDescription, utilStrList.Strings[i]) = true) then
    begin
      locDescription[0] := locDescription[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(blockDefault_Categories, utilStrList.Strings[i]) = true) then
    begin
      locDefault_Categories[0] := locDefault_Categories[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(blockSettings, utilStrList.Strings[i]) = true) then
    begin
      locSettings[0] := locSettings[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(blockDefault_Text, utilStrList.Strings[i]) = true) then
    begin
      locDefault_Text[0] := locDefault_Text[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(blockOne_Line, utilStrList.Strings[i]) = true) then
    begin
      locOne_Line[0] := locOne_Line[0] + 1; // If we've got a match, increase the count
    end;

    // Add further block element searches here...
  end;

  // Set the array lengths based upon the above
  SetLength(locLogbook_Type, locLogbook_Type[0]+1);
  SetLength(locDescription, locDescription[0]+1);
  SetLength(locDefault_Categories, locDefault_Categories[0]+1);
  SetLength(locSettings, locSettings[0]+1);
  SetLength(locDefault_Text, locDefault_Text[0]+1);
  SetLength(locOne_Line, locOne_Line[0]+1);

  // Reset the counters
  locLogbook_Type[0] := 0;
  locDescription[0] := 0;
  locDefault_Categories[0] := 0;
  locSettings[0] := 0;
  locDefault_Text[0] := 0;
  locOne_Line[0] := 0;

  // The, run through the entire file again for each instance of each block
  for i := 0 to utilStrList.Count-1 do
  begin

    if (AnsiContainsText(blockLogbook_Type, utilStrList.Strings[i]) = true) then
    begin
      locLogbook_Type[0] := locLogbook_Type[0] + 1; // If we've got a match, increase the count
      locLogbook_Type[locLogbook_Type[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    if (AnsiContainsText(blockDescription, utilStrList.Strings[i]) = true) then
    begin
      locDescription[0] := locDescription[0] + 1; // If we've got a match, increase the count
      locDescription[locDescription[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    if (AnsiContainsText(blockDefault_Categories, utilStrList.Strings[i]) = true) then
    begin
      locDefault_Categories[0] := locDefault_Categories[0] + 1; // If we've got a match, increase the count
      locDefault_Categories[locDefault_Categories[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    if (AnsiContainsText(blockSettings, utilStrList.Strings[i]) = true) then
    begin
      locSettings[0] := locSettings[0] + 1; // If we've got a match, increase the count
      locSettings[locSettings[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    if (AnsiContainsText(blockDefault_Text, utilStrList.Strings[i]) = true) then
    begin
      locDefault_Text[0] := locDefault_Text[0] + 1; // If we've got a match, increase the count
      locDefault_Text[locDefault_Text[0]] := i; // Add the new value to the array showing the location of a block element
    end;

    if (AnsiContainsText(blockOne_Line, utilStrList.Strings[i]) = true) then
    begin
      locOne_Line[0] := locOne_Line[0] + 1; // If we've got a match, increase the count
      locOne_Line[locOne_Line[0]] := i; // Add the new value to the array showing the location of a block element
    end;

  // Add further block element searches here...
  end;


  // Check to ensure each block has an even number of elements
  hasError := False;
  errorMsg := '';

  if ((Length(locLogbook_Type)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockLogbook_Type + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;

  if ((Length(locDescription)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockDescription + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;

  if ((Length(locDefault_Categories)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockDefault_Categories + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;

  if ((Length(locSettings)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockSettings + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;

  if ((Length(locDefault_Text)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockDefault_Text + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;

  if ((Length(locOne_Line)-1) mod 2 <> 0) then
  begin
    errorMsg := 'There is an error in the "' + blockOne_Line + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    hasError := True;
  end;




  // Then process the blocks into their respective variables (String or TStringList)
  if errorMsg = '' then // If there's no error, keep on processing
  begin

    // Logbook_Type is a special case since only one entry is allowed, and only one line is allowed
    Logbook_Type := utilStrList.Strings[locLogbook_Type[1]+1];

    // Add the Description, which is a single multi-line block (additional intances of Description are ignored)
    for i := locDescription[1]+1 to locDescription[2]-1 do
    begin
      Description.AddText(utilStrList.Strings[i]);
    end;

    // Add the Default_Categories, which is a single multi-line block (additional intances of Default_Categories are ignored)
    for i := locDefault_Categories[1]+1 to locDefault_Categories[2]-1 do
    begin
      Default_Categories.AddText(utilStrList.Strings[i]);
    end;

    // Add the Settings, which is a single multi-line block (additional intances of Settings are ignored)
    for i := locSettings[1]+1 to locSettings[2]-1 do
    begin
      Settings.AddText(utilStrList.Strings[i]);
    end;

    // Add the Default_Text, which is a single multi-line block (additional intances of Default_Text are ignored)
    for i := locDefault_Text[1]+1 to locDefault_Text[2]-1 do
    begin
      Default_Text.AddText(utilStrList.Strings[i]);
    end;

    // Add the One_Line, which is a single multi-line block (additional intances of One_Line are ignored)
    for i := locOne_Line[1]+1 to locOne_Line[2]-1 do
    begin
      One_Line.AddText(utilStrList.Strings[i]);
    end;


  end;

  utilStrList.Free;
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
end;


end.

