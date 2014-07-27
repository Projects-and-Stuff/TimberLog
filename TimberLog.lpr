program TimberLog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, richmemopackage, uniqueinstance_package, sqlite3laz, rx,
  zvdatetimectrls, formUnitStartDialog, unitStartFunctions, dmUnitDBTools,
  unitClassLogbook, unitRecordLogMetadata, unitDefinitions, formUnitLogbook,
  dmUnitCrypt, unitTypeTile, unitRecentTile;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TformStartDialog, formStartDialog);
  Application.CreateForm(TformLogbook, formLogbook);
  Application.CreateForm(TdmDBTools, dmDBTools);
  Application.CreateForm(TdmCrypt, dmCrypt);
  Application.Run;
end.

