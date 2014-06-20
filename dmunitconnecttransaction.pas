unit dmUnitConnectTransaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, FileUtil;

type

  { TdmConnectTransaction }

  TdmConnectTransaction = class(TDataModule)
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dmConnectTransaction: TdmConnectTransaction;

implementation

{$R *.lfm}

end.

