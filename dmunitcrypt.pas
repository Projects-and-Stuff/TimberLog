unit dmUnitCrypt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DCPtwofish, DCPrc4, DCPsha256, DCPsha1;

type

  { TdmCrypt }

  TdmCrypt = class(TDataModule)
    DCP_sha256: TDCP_sha256;
    DCP_twofish: TDCP_twofish;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure stringHash(input : String; out output : String);
    procedure fileHash(filepath : String; out output : String);
    function newSalt(): String;
    procedure encryptString(keystring : String; var input: String);
    procedure decryptString(keystring : String; var input: String);
    function encryptFile(keystring : String; filepath: String): Boolean;
    function decryptFile(keystring : String; filepath: String): Boolean;
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

procedure TdmCrypt.stringHash(input: String; out output: String);
var
  Digest : array[0..63] of byte;  // 256bit digest (64 bytes)
  i : Integer;
  hashOut : String;
begin
  DCP_sha256.Init;
  DCP_sha256.UpdateStr(input);
  DCP_sha256.Final(Digest);
  hashOut := '';

  for i := 0 to 15 do
  begin
    hashOut := hashOut + IntToHex(Digest[i],2);
  end;

  SetLength(hashOut, 64);
  output := hashOut;
end;

procedure TdmCrypt.fileHash(filepath: String; out output: String);
var
  Digest : array[0..63] of byte;  // 256bit digest (64 bytes)
  i : Integer;
  hashOut : String;
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
    DCP_sha256.Init;                                    // initialize it
    DCP_sha256.UpdateStream(Source, Source.Size);       // hash the stream contents
    DCP_sha256.Final(Digest);                           // produce the digest

    hashOut := '';
    for i := 0 to 19 do
    begin
      hashOut := hashOut + IntToHex(Digest[i],2);
    end;

    SetLength(hashOut, 64);
    output := hashOut;
  end;

  Source.Free;
end;

function TdmCrypt.newSalt(): String;
begin
  stringHash(IntToStr(Random(2000000000)), newSalt);
end;

procedure TdmCrypt.encryptString(keystring : String; var input: String);
begin
  DCP_twofish.InitStr(keystring, TDCP_sha256);         // initialize the cipher with a hash of the passphrase
  input := DCP_twofish.EncryptString(input);         // encrypt the contents of the memo
  DCP_twofish.Burn;
end;

procedure TdmCrypt.decryptString(keystring : String; var input: String);
begin
  DCP_twofish.InitStr(keystring, TDCP_sha256);         // initialize the cipher with a hash of the passphrase
  input:= DCP_twofish.DecryptString(input);          // decrypt the contents of the memo
  DCP_twofish.Burn;
end;

function TdmCrypt.encryptFile(keystring: String; filepath: String): Boolean;
var
  Source, Dest: TFileStream;
begin
  try
    Source:= TFileStream.Create(filepath, fmOpenRead);
    Dest:= TFileStream.Create(ChangeFileExt(filepath, '.bak'),fmCreate);
    DCP_twofish.InitStr(keystring, TDCP_sha256);              // initialize the cipher with a hash of the passphrase
    DCP_twofish.EncryptStream(Source, Dest, Source.Size); // encrypt the contents of the file
    DCP_twofish.Burn;
    Dest.Free;
    Source.Free;
    DeleteFile(filepath);
    RenameFile(ChangeFileExt(filepath, '.bak'), ChangeFileExt(filepath, '.logb'));
    encryptFile := True;
  except
    encryptFile := False;
  end;
end;

function TdmCrypt.decryptFile(keystring: String; filepath: String): Boolean;
var
  Source, Dest: TFileStream;
begin
  try
    Source:= TFileStream.Create(filepath, fmOpenRead);
    Dest:= TFileStream.Create(ChangeFileExt(filepath, '.bak'),fmCreate);
    DCP_twofish.InitStr(keystring, TDCP_sha256);              // initialize the cipher with a hash of the passphrase
    DCP_twofish.DecryptStream(Source, Dest, Source.Size); // decrypt the contents of the file
    DCP_twofish.Burn;
    Dest.Free;
    Source.Free;
    DeleteFile(filepath);
    RenameFile(ChangeFileExt(filepath, '.bak'), ChangeFileExt(filepath, '.logb'));
    decryptFile := True;
  except
    decryptFile := False;
  end;
end;

end.

