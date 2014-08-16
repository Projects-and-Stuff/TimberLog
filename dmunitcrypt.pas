unit dmUnitCrypt;

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DCPtwofish, DCPrc4, DCPsha256, DCPsha1;

type

  { TdmCrypt }

  TdmCrypt = class(TDataModule)
    DCP_SHA256: TDCP_sha256;
    DCP_Twofish: TDCP_twofish;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure StringHash(input : String; out output : String);
    procedure FileHash(filepath : String; out output : String);
    function NewSalt: String;
    procedure EncryptString(keystring : String; var input: String);
    procedure DecryptString(keystring : String; var input: String);
    function EncryptFile(keystring : String; filepath: String): Boolean;
    function DecryptFile(keystring : String; filepath: String): Boolean;
  end;

var
  dmCrypt: TdmCrypt;

implementation

{$R *.lfm}

{ TdmCrypt }

procedure TdmCrypt.DataModuleCreate(Sender: TObject);
begin
  Randomize;
end;

procedure TdmCrypt.StringHash(input: String; out output: String);
var
  Digest : array[0..63] of byte;  // 256bit digest (64 bytes)
  i : Integer;
  HashOut : String;
begin
  DCP_SHA256.Init;
  DCP_SHA256.UpdateStr(input);
  DCP_SHA256.Final(Digest);
  HashOut := '';

  for i := 0 to 15 do
  begin
    HashOut := HashOut + IntToHex(Digest[i],2);
  end;

  SetLength(HashOut, 64);
  output := HashOut;
end;

procedure TdmCrypt.FileHash(filepath: String; out output: String);
var
  Digest : array[0..63] of byte;  // 256bit digest (64 bytes)
  i : Integer;
  HashOut : String;
  Source : TFileStream;
begin
  Source := Nil;
  try
    Source:= TFileStream.Create(filepath, fmOpenRead);  // open the file
  except
    // Report errors
  end;

  if Source <> nil then
  begin
    DCP_SHA256.Init;                                    // initialize it
    DCP_SHA256.UpdateStream(Source, Source.Size);       // hash the stream contents
    DCP_SHA256.Final(Digest);                           // produce the digest

    HashOut := '';
    for i := 0 to 19 do
    begin
      HashOut := HashOut + IntToHex(Digest[i],2);
    end;

    SetLength(HashOut, 64);
    output := HashOut;
  end;

  Source.Free;
end;

function TdmCrypt.NewSalt: String;
begin
  StringHash(IntToStr(Random(2000000000)), NewSalt);
end;

procedure TdmCrypt.EncryptString(KeyString : String; var Input: String);
begin
  DCP_Twofish.InitStr(KeyString, TDCP_sha256);         // initialize the cipher with a hash of the passphrase
  Input := DCP_Twofish.EncryptString(Input);         // encrypt the contents of the memo
  DCP_Twofish.Burn;
end;

procedure TdmCrypt.DecryptString(KeyString : String; var Input: String);
begin
  DCP_Twofish.InitStr(KeyString, TDCP_sha256);         // initialize the cipher with a hash of the passphrase
  input:= DCP_Twofish.DecryptString(Input);          // decrypt the contents of the memo
  DCP_Twofish.Burn;
end;

function TdmCrypt.EncryptFile(KeyString: String; Filepath: String): Boolean;
var
  Source, Dest: TFileStream;
begin
  try
    Source:= TFileStream.Create(Filepath, fmOpenRead);
    Dest:= TFileStream.Create(ChangeFileExt(Filepath, '.bak'),fmCreate);
    DCP_Twofish.InitStr(KeyString, TDCP_sha256);              // initialize the cipher with a hash of the passphrase
    DCP_Twofish.EncryptStream(Source, Dest, Source.Size); // encrypt the contents of the file
    DCP_Twofish.Burn;
    Dest.Free;
    Source.Free;
    DeleteFile(Filepath);
    RenameFile(ChangeFileExt(Filepath, '.bak'), ChangeFileExt(Filepath, '.logb'));
    EncryptFile := True;
  except
    EncryptFile := False;
  end;
end;

function TdmCrypt.DecryptFile(KeyString: String; Filepath: String): Boolean;
var
  Source, Dest: TFileStream;
begin
  try
    Source:= TFileStream.Create(Filepath, fmOpenRead);
    Dest:= TFileStream.Create(ChangeFileExt(Filepath, '.bak'),fmCreate);
    DCP_Twofish.InitStr(KeyString, TDCP_sha256);              // initialize the cipher with a hash of the passphrase
    DCP_Twofish.DecryptStream(Source, Dest, Source.Size); // decrypt the contents of the file
    DCP_Twofish.Burn;
    Dest.Free;
    Source.Free;
    DeleteFile(Filepath);
    RenameFile(ChangeFileExt(Filepath, '.bak'), ChangeFileExt(Filepath, '.logb'));
    DecryptFile := True;
  except
    DecryptFile := False;
  end;
end;

end.

