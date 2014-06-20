unit dmUnitSourceQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil;

type

  { TdmSourceQuery }

  TdmSourceQuery = class(TDataModule)
    DataSource1: TDataSource;
    SQLQuery1: TSQLQuery;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dmSourceQuery: TdmSourceQuery;

implementation

{$R *.lfm}

end.

