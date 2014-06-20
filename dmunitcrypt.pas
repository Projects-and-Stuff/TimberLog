unit dmUnitCrypt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DCPtwofish, DCPsha512;

type

  { TdmCrypt }

  TdmCrypt = class(TDataModule)
    DCP_sha512: TDCP_sha512;
    DCP_twofish: TDCP_twofish;
  private
    { private declarations }
  public
    { public declarations }
    function hash(input : String): String;
    function cryptString(input : String): String;
  end;

var
  dmCrypt: TdmCrypt;

implementation

{$R *.lfm}

{ TdmCrypt }

function TdmCrypt.hash(input: String): String;
var
  Digest : array[0..63] of byte;  // 512bit digest (64 bytes)
  i : integer;
  hashOut : string;
begin
  Randomize;
  DCP_sha512.Init;
  DCP_sha512.UpdateStr(input);
  DCP_sha512.Final(Digest);
  hashOut := '';
    for i := 0 to 15 do
    begin
      hashOut := hashOut + IntToHex(Digest[i],2);
    end;

    hash := hashOut;
end;

function TdmCrypt.cryptString(input: String): String;
begin

end;

end.

