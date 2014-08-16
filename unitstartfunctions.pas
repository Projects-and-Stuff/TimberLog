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
  Classes, SysUtils, shlobj, {unitRecordLogMetadata,} strutils, unitDefinitions,
  {dmUnitCrypt,} Controls{, StdCtrls}. LazLogger;

  procedure ResetPages; // Resets Page 1 to the default values
  function ReturnDocumentsPath : String;
  procedure ProcessTypeFile(filepath : String; out ErrorMsg : String; out Logbook_Type : String; Description : TStringList); // Short form used to identify the type name and description
  procedure ProcessTypeFile(filepath : String; out ErrorMsg : String; out Logbook_Type : String; Description : TStringList;  // Short form used to identify the type name, description, and categories
    Default_Categories : TStringList);
  procedure ProcessTypeFile(filepath : String; out ErrorMsg : String; out Logbook_Type : String; Description : TStringList; // Full form with entire type definition
    Settings: TStringList; Default_Categories: TStringList; Default_Text: TStringList;
    OneLine: TStringList; Multi_Line: TStringList);
  procedure GetTypeFileList(var fileList : TStringList);
  procedure Split(const Delimiter : Char; Input : String; const Strings : TStrings);

implementation

uses
  formUnitStartDialog;

procedure Split(const Delimiter : Char; Input : String; const Strings : TStrings);
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.Split'); {$endif}

  Assert(Assigned(Strings)) ;
  Strings.Clear;
  Strings.StrictDelimiter := True;
  Strings.Delimiter := Delimiter;
  Strings.DelimitedText := Input;
end;

procedure ResetPages;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.ResetPages'); {$endif}

  //formStartDialog.Notebook1.PageIndex := 0; // Reset to the default page on formStartDialog

  // Clear out all the components on page 1
  formStartDialog.txtLogName.Clear;
  formStartDialog.memoLogDescription.Clear;
  formStartDialog.chkPassMaster.Checked := False;
  formStartDialog.chkPassToExport.Checked := False;
  formStartDialog.chkPassToPrint.Checked := False;
  formStartDialog.chkPassPerUser.Checked := False;
  formStartDialog.txtMasterPass.Clear;
  formStartDialog.txtMasterPass1.Clear;
  formStartDialog.txtPassToExport.Clear;
  formStartDialog.txtPassToExport1.Clear;
  formStartDialog.txtPassToPrint.Clear;
  formStartDialog.txtPassToPrint1.Clear;
  formStartDialog.txtOpenedBy.Clear;
  formStartDialog.cmbDFormat.ItemIndex := 0;
  formStartDialog.cmbTFormat.ItemIndex := 0;
  formStartDialog.chkAllowCategories.Checked := False;
  formStartDialog.chkAllowAddCategories.Checked := False;
  formStartDialog.txtCategory.Clear;
  formStartDialog.listboxCategories.Clear;

  // Reset the path for ShellTreeView on page 2
  try
    if not DirectoryExists(ReturnDocumentsPath + '\TimberLog\') then  // Check if TimberLog user directory exists
    begin
      MkDir(ReturnDocumentsPath + '\TimberLog\');
    end;

    formStartDialog.shellTreeSelectFolder.Path := ReturnDocumentsPath + '\TimberLog\';
  finally

  end;

end;

function ReturnDocumentsPath: String;
var
  DocumentsPath: Array[0..MaxPathLen] of Char; //Allocate memory
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.ReturnDocumentsPath'); {$endif}

  DocumentsPath := '';
  SHGetSpecialFolderPath(0,DocumentsPath,CSIDL_PERSONAL,False); // http://delphi-miranda-plugins.googlecode.com/svn-history/r105/trunk/FPC/units/src/shlobj.pp
  ReturnDocumentsPath := DocumentsPath;
end;






///////////////////////////////////////////////////////////////////////////////////////////////////
// ToDo: Consider splitting these various actions into separate procedures for better modularity //
///////////////////////////////////////////////////////////////////////////////////////////////////

procedure ProcessTypeFile(Filepath : String; out ErrorMsg : String; out Logbook_Type: String; Description: TStringList);
var
  UtilStrList : TStringList; // Holds the entire Type File while we process it

  // These Loc arrays are used for identifying the type data
  // The first value (IE: LocType[0]) counts how many instances there are so far
  // All additional values specify the Location of the Blocks
  LocLogbook_Type, LocDescription : array of Integer;

  i : Integer;
  HasError : Boolean; // Used to check whether there is an error in number of Block elements
begin

  UtilStrList := TStringList.Create;
  UtilStrList.LoadFromFile(Filepath); // Load up the type file into the utility TStringList

  // Set the initial length of Locating arrays
  SetLength(LocLogbook_Type, 1);
  SetLength(LocDescription, 1);

  // Initially, there are no instances of each type Block
  LocLogbook_Type[0] := 0;
  LocDescription[0] := 0;


  // Initially, run through the entire file looking for the total number of each Block
  for i := 0 to UtilStrList.Count-1 do
  begin

    if (AnsiContainsText(BlockLogbook_Type, uUilStrList.Strings[i]) = True) then
    begin
      LocLogbook_Type[0] := LocLogbook_Type[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(BlockDescription, UtilStrList.Strings[i]) = True) then
    begin
      LocDescription[0] := LocDescription[0] + 1; // If we've got a match, increase the count
    end;

    // Add further Block element searches here...
  end;

  // Set the array lengths based upon the above
  SetLength(LocLogbook_Type, LocLogbook_Type[0]+1);
  SetLength(LocDescription, LocDescription[0]+1);

  // Reset the counters
  LocLogbook_Type[0] := 0;
  LocDescription[0] := 0;

  // The, run through the entire file again for each instance of each Block
  for i := 0 to UtilStrList.Count-1 do
  begin

    if (AnsiContainsText(BlockLogbook_Type, UtilStrList.Strings[i]) = True) then
    begin
      LocLogbook_Type[0] := LocLogbook_Type[0] + 1; // If we've got a match, increase the count
      LocLogbook_Type[LocLogbook_Type[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

    if (AnsiContainsText(BlockDescription, UtilStrList.Strings[i]) = True) then
    begin
      LocDescription[0] := LocDescription[0] + 1; // If we've got a match, increase the count
      LocDescription[LocDescription[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

  // Add further Block element searches here...
  end;


  // Check to ensure each Block has an even number of elements
  HasError := False;
  ErrorMsg := '';

  if ((Length(LocLogbook_Type)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockLogbook_Type + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;

  if ((Length(LocDescription)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockDescription + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;



  // Then process the Blocks into their respective variables (String or TStringList)
  if ErrorMsg = '' then // If there's no error, keep on processing
  begin

    // Logbook_Type is a special case since only one entry is allowed, and only one line is allowed
    Logbook_Type := UtilStrList.Strings[LocLogbook_Type[1]+1];

    // Add the description, which is a single multi-line Block (additional intances of Description are ignored)
    for i := LocDescription[1]+1 to LocDescription[2]-1 do
    begin
      Description.AddText(UtilStrList.Strings[i]);
    end;

  end;

  UtilStrList.Free;
end;

procedure processTypeFile(filepath : String; out ErrorMsg : String; out Logbook_Type: String; Description: TStringList;
  Default_Categories: TStringList);
var
  UtilStrList : TStringList; // Holds the entire Type File while we process it

  // These Loc arrays are used for identifying the type data
  // The first value (IE: LocType[0]) counts how many instances there are so far
  // All additional values specify the Location of the Blocks
  LocLogbook_Type, LocDescription, LocDefaultCategories : array of Integer;

  i : Integer;
  HasError : Boolean; // Used to check whether there is an error in number of Block elements
begin
  UtilStrList := TStringList.Create;
  UtilStrList.LoadFromFile(Filepath); // Load up the type file into the utility TStringList

  // Set the initial length of Locating arrays
  SetLength(LocLogbook_Type, 1);
  SetLength(LocDescription, 1);
  SetLength(LocDefaultCategories, 1);

  // Initially, there are no instances of each type Block
  LocLogbook_Type[0] := 0;
  LocDescription[0] := 0;
  LocDefaultCategories[0] := 0;


  // Initially, run through the entire file looking for the total number of each Block
  for i := 0 to UtilStrList.Count-1 do
  begin

    if (AnsiContainsText(BlockLogbook_Type, UtilStrList.Strings[i]) = True) then
    begin
      LocLogbook_Type[0] := LocLogbook_Type[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(BlockDescription, UtilStrList.Strings[i]) = True) then
    begin
      LocDescription[0] := LocDescription[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(BlockDefault_Categories, UtilStrList.Strings[i]) = True) then
    begin
      LocDefaultCategories[0] := LocDefaultCategories[0] + 1; // If we've got a match, increase the count
    end;

    // Add further Block element searches here...
  end;

  // Set the array lengths based upon the above
  SetLength(LocLogbook_Type, LocLogbook_Type[0]+1);
  SetLength(LocDescription, LocDescription[0]+1);
  SetLength(LocDefaultCategories, LocDefaultCategories[0]+1);

  // Reset the counters
  LocLogbook_Type[0] := 0;
  LocDescription[0] := 0;
  LocDefaultCategories[0] := 0;

  // The, run through the entire file again for each instance of each Block
  for i := 0 to UtilStrList.Count-1 do
  begin

    if (AnsiContainsText(BlockLogbook_Type, UtilStrList.Strings[i]) = True) then
    begin
      LocLogbook_Type[0] := LocLogbook_Type[0] + 1; // If we've got a match, increase the count
      LocLogbook_Type[LocLogbook_Type[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

    if (AnsiContainsText(BlockDescription, UtilStrList.Strings[i]) = True) then
    begin
      LocDescription[0] := LocDescription[0] + 1; // If we've got a match, increase the count
      LocDescription[LocDescription[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

    if (AnsiContainsText(BlockDefault_Categories, UtilStrList.Strings[i]) = True) then
    begin
      LocDefaultCategories[0] := LocDefaultCategories[0] + 1; // If we've got a match, increase the count
      LocDefaultCategories[LocDefaultCategories[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

  // Add further Block element searches here...
  end;


  // Check to ensure each Block has an even number of elements
  HasError := False;
  ErrorMsg := '';

  if ((Length(LocLogbook_Type)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockLogbook_Type + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;

  if ((Length(LocDescription)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockDescription + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;

  if ((Length(LocDefaultCategories)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockDefault_Categories + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;



  // Then process the Blocks into their respective variables (String or TStringList)
  if ErrorMsg = '' then // If there's no error, keep on processing
  begin

    // Logbook_Type is a special case since only one entry is allowed, and only one line is allowed
    Logbook_Type := UtilStrList.Strings[LocLogbook_Type[1]+1];

    // Add the Description, which is a single multi-line Block (additional intances of Description are ignored)
    for i := LocDescription[1]+1 to LocDescription[2]-1 do
    begin
      Description.AddText(UtilStrList.Strings[i]);
    end;

    // Add the Default_Categories, which is a single multi-line Block (additional intances of Default_Categories are ignored)
    for i := LocDefaultCategories[1]+1 to LocDefaultCategories[2]-1 do
    begin
      Default_Categories.AddText(UtilStrList.Strings[i]);
    end;

  end;

  UtilStrList.Free;
end;

procedure processTypeFile(filepath : String; out ErrorMsg : String; out Logbook_Type: String; Description: TStringList;
  Settings : TStringList; Default_Categories : TStringList; Default_Text: TStringList; OneLine: TStringList; Multi_Line: TStringList);
var
  UtilStrList : TStringList; // Holds the entire Type File while we process it

  // These Loc arrays are used for identifying the type data
  // The first value (IE: LocType[0]) counts how many instances there are so far
  // All additional values specify the Location of the Blocks
  LocLogbook_Type, LocDescription, LocSettings, LocDefaultCategories, LocDefault_Text, LocOneLine, LocMulti_Line : array of Integer;

  i : Integer;
  HasError : Boolean; // Used to check whether there is an error in number of Block elements
begin
  UtilStrList := TStringList.Create;
  UtilStrList.LoadFromFile(filepath); // Load up the type file into the utility TStringList

  // Set the initial length of Locating arrays
  SetLength(LocLogbook_Type, 1);
  SetLength(LocDescription, 1);
  SetLength(LocDefaultCategories, 1);
  SetLength(LocSettings, 1);
  SetLength(LocDefault_Text, 1);
  SetLength(LocOneLine, 1);

  // Initially, there are no instances of each type Block
  LocLogbook_Type[0] := 0;
  LocDescription[0] := 0;
  LocDefaultCategories[0] := 0;
  LocSettings[0] := 0;
  LocDefault_Text[0] := 0;
  LocOneLine[0] := 0;


  // Initially, run through the entire file looking for the total number of each Block
  for i := 0 to UtilStrList.Count-1 do
  begin

    if (AnsiContainsText(BlockLogbook_Type, UtilStrList.Strings[i]) = True) then
    begin
      LocLogbook_Type[0] := LocLogbook_Type[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(BlockDescription, UtilStrList.Strings[i]) = True) then
    begin
      LocDescription[0] := LocDescription[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(BlockDefault_Categories, UtilStrList.Strings[i]) = True) then
    begin
      LocDefaultCategories[0] := LocDefaultCategories[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(BlockSettings, UtilStrList.Strings[i]) = True) then
    begin
      LocSettings[0] := LocSettings[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(BlockDefault_Text, UtilStrList.Strings[i]) = True) then
    begin
      LocDefault_Text[0] := LocDefault_Text[0] + 1; // If we've got a match, increase the count
    end;

    if (AnsiContainsText(BlockOneLine, UtilStrList.Strings[i]) = True) then
    begin
      LocOneLine[0] := LocOneLine[0] + 1; // If we've got a match, increase the count
    end;

    // Add further Block element searches here...
  end;

  // Set the array lengths based upon the above
  SetLength(LocLogbook_Type, LocLogbook_Type[0]+1);
  SetLength(LocDescription, LocDescription[0]+1);
  SetLength(LocDefaultCategories, LocDefaultCategories[0]+1);
  SetLength(LocSettings, LocSettings[0]+1);
  SetLength(LocDefault_Text, LocDefault_Text[0]+1);
  SetLength(LocOneLine, LocOneLine[0]+1);

  // Reset the counters
  LocLogbook_Type[0] := 0;
  LocDescription[0] := 0;
  LocDefaultCategories[0] := 0;
  LocSettings[0] := 0;
  LocDefault_Text[0] := 0;
  LocOneLine[0] := 0;

  // The, run through the entire file again for each instance of each Block
  for i := 0 to UtilStrList.Count-1 do
  begin

    if (AnsiContainsText(blockLogbook_Type, UtilStrList.Strings[i]) = True) then
    begin
      LocLogbook_Type[0] := LocLogbook_Type[0] + 1; // If we've got a match, increase the count
      LocLogbook_Type[LocLogbook_Type[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

    if (AnsiContainsText(blockDescription, UtilStrList.Strings[i]) = True) then
    begin
      LocDescription[0] := LocDescription[0] + 1; // If we've got a match, increase the count
      LocDescription[LocDescription[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

    if (AnsiContainsText(blockDefault_Categories, UtilStrList.Strings[i]) = True) then
    begin
      LocDefaultCategories[0] := LocDefaultCategories[0] + 1; // If we've got a match, increase the count
      LocDefaultCategories[LocDefaultCategories[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

    if (AnsiContainsText(blockSettings, UtilStrList.Strings[i]) = True) then
    begin
      LocSettings[0] := LocSettings[0] + 1; // If we've got a match, increase the count
      LocSettings[LocSettings[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

    if (AnsiContainsText(blockDefault_Text, UtilStrList.Strings[i]) = True) then
    begin
      LocDefault_Text[0] := LocDefault_Text[0] + 1; // If we've got a match, increase the count
      LocDefault_Text[LocDefault_Text[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

    if (AnsiContainsText(blockOneLine, UtilStrList.Strings[i]) = True) then
    begin
      LocOneLine[0] := LocOneLine[0] + 1; // If we've got a match, increase the count
      LocOneLine[LocOneLine[0]] := i; // Add the new value to the array showing the Location of a Block element
    end;

  // Add further Block element searches here...
  end;


  // Check to ensure each Block has an even number of elements
  HasError := False;
  ErrorMsg := '';

  if ((Length(LocLogbook_Type)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockLogbook_Type + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;

  if ((Length(LocDescription)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockDescription + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;

  if ((Length(LocDefaultCategories)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockDefault_Categories + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;

  if ((Length(LocSettings)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockSettings + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;

  if ((Length(LocDefault_Text)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockDefault_Text + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;

  if ((Length(LocOneLine)-1) mod 2 <> 0) then
  begin
    ErrorMsg := 'There is an error in the "' + BlockOneLine + '" element of this Logbook Type file. Use the Type Editor to identify this and any other problems.';
    HasError := True;
  end;




  // Then process the Blocks into their respective variables (String or TStringList)
  if ErrorMsg = '' then // If there's no error, keep on processing
  begin

    // Logbook_Type is a special case since only one entry is allowed, and only one line is allowed
    Logbook_Type := UtilStrList.Strings[LocLogbook_Type[1]+1];

    // Add the Description, which is a single multi-line Block (additional intances of Description are ignored)
    for i := LocDescription[1]+1 to LocDescription[2]-1 do
    begin
      Description.AddText(UtilStrList.Strings[i]);
    end;

    // Add the Default_Categories, which is a single multi-line Block (additional intances of Default_Categories are ignored)
    for i := LocDefaultCategories[1]+1 to LocDefaultCategories[2]-1 do
    begin
      Default_Categories.AddText(UtilStrList.Strings[i]);
    end;

    // Add the Settings, which is a single multi-line Block (additional intances of Settings are ignored)
    for i := LocSettings[1]+1 to LocSettings[2]-1 do
    begin
      Settings.AddText(UtilStrList.Strings[i]);
    end;

    // Add the Default_Text, which is a single multi-line Block (additional intances of Default_Text are ignored)
    for i := LocDefault_Text[1]+1 to LocDefault_Text[2]-1 do
    begin
      Default_Text.AddText(UtilStrList.Strings[i]);
    end;

    // Add the OneLine, which is a single multi-line Block (additional intances of OneLine are ignored)
    for i := LocOneLine[1]+1 to LocOneLine[2]-1 do
    begin
      OneLine.AddText(UtilStrList.Strings[i]);
    end;


  end;

  UtilStrList.Free;
end;

procedure GetTypeFileList(var fileList: TStringList);
var
  Info : TSearchRec;
  AnyExists : Boolean;
begin
  {$ifdef dbgTimberLog} DebugLn(ClassName, '.getTypeFileList'); {$endif}

  AnyExists := False;

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
          AyExists := True;
          FileList.AddText(GetAppConfigDir(True) + Name);
        end;
      end;

    Until FindNext(Info)<>0;
  end;

  if AnyExists = False then
  begin
    FileList.AddText('None');
  end;

  FindClose(Info); // Close our search
end;


end.

