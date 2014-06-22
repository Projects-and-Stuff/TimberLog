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
  private
    { private declarations }
  public
    { public declarations }
    function hash(input : String): String;
    procedure encryptString(keystring : String; var input: String);
    procedure decryptString(keystring : String; var input: String);
    function encryptFile(keystring : String; inputFileName: String): Boolean;
    function decryptFile(keystring : String; inputFileName: String): Boolean;
  end;

var
  dmCrypt: TdmCrypt;

implementation

{$R *.lfm}

{ TdmCrypt }

function TdmCrypt.hash(input: String): String;
var
  Digest : array[0..63] of byte;  // 256bit digest (64 bytes)
  i : integer;
  hashOut : string;
begin
  Randomize;
  DCP_sha256.Init;
  DCP_sha256.UpdateStr(input);
  DCP_sha256.Final(Digest);
  hashOut := '';
    for i := 0 to 15 do
    begin
      hashOut := hashOut + IntToHex(Digest[i],2);
    end;

    hash := hashOut;
end;

procedure TdmCrypt.encryptString(keystring : String; var input: String);
begin
  Randomize;
  DCP_twofish.InitStr(keystring, TDCP_sha256);         // initialize the cipher with a hash of the passphrase
  input := DCP_twofish.EncryptString(input);         // encrypt the contents of the memo
  DCP_twofish.Burn;
end;

procedure TdmCrypt.decryptString(keystring : String; var input: String);
begin
  Randomize;
  DCP_twofish.InitStr(keystring, TDCP_sha256);         // initialize the cipher with a hash of the passphrase
  input:= DCP_twofish.DecryptString(input);          // decrypt the contents of the memo
  DCP_twofish.Burn;
end;

function TdmCrypt.encryptFile(keystring: String; inputFileName: String): Boolean;
var
  Source, Dest: TFileStream;
begin
  try
    Randomize;
    Source:= TFileStream.Create(inputFileName,fmOpenRead);
    Dest:= TFileStream.Create(ChangeFileExt(inputFileName, '.bak'),fmCreate);
    DCP_twofish.InitStr(keystring,TDCP_sha256);              // initialize the cipher with a hash of the passphrase
    DCP_twofish.EncryptStream(Source,Dest,Source.Size); // encrypt the contents of the file
    DCP_twofish.Burn;
    Dest.Free;
    Source.Free;
    DeleteFile(inputFileName);
    RenameFile(ChangeFileExt(inputFileName, '.bak'), ChangeFileExt(inputFileName, '.logb'));
    encryptFile := True;
  except
    encryptFile := False;
  end;
end;

function TdmCrypt.decryptFile(keystring: String; inputFileName: String): Boolean;
var
  Source, Dest: TFileStream;
begin
  try
    Randomize;
    Source:= TFileStream.Create(inputFileName,fmOpenRead);
    Dest:= TFileStream.Create(ChangeFileExt(inputFileName, '.bak'),fmCreate);
    DCP_twofish.InitStr(keystring,TDCP_sha256);              // initialize the cipher with a hash of the passphrase
    DCP_twofish.DecryptStream(Source,Dest,Source.Size); // decrypt the contents of the file
    DCP_twofish.Burn;
    Dest.Free;
    Source.Free;
    DeleteFile(inputFileName);
    RenameFile(ChangeFileExt(inputFileName, '.bak'), ChangeFileExt(inputFileName, '.logb'));
    decryptFile := True;
  except
    decryptFile := False;
  end;
end;

end.

