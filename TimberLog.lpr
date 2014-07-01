program TimberLog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, richmemopackage, sqlite3laz, zvdatetimectrls, formUnitStartDialog,
  unitStartFunctions, dmUnitConnectTransaction, dmUnitSourceQuery,
  unitClassLogbook, unitRecordLogMetadata, unitDefinitions, formUnitLogbook,
  unitRecordLogMetadataExt, dmUnitCrypt, unitTypeTile, unitRecentTile
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TformStartDialog, formStartDialog);
  Application.CreateForm(TdmConnectTransaction, dmConnectTransaction);
  Application.CreateForm(TdmSourceQuery, dmSourceQuery);
  Application.CreateForm(TformLogbook, formLogbook);
  Application.CreateForm(TdmCrypt, dmCrypt);
  Application.Run;
end.

