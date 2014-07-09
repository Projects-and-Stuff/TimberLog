program TimberLog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, richmemopackage, uniqueinstance_package, sqlite3laz, zvdatetimectrls,
  formUnitStartDialog, unitStartFunctions, dmUnitDBTools, unitClassLogbook,
  unitRecordLogMetadata, unitDefinitions, formUnitLogbook, dmUnitCrypt,
  unitTypeTile, unitRecentTile
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TdmDBTools, dmDBTools);
  Application.CreateForm(TformStartDialog, formStartDialog);
  Application.CreateForm(TformLogbook, formLogbook);
  Application.CreateForm(TdmCrypt, dmCrypt);
  Application.Run;
end.

