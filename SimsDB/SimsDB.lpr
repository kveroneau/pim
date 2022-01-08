program SimsDB;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, dbwindow
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='The Sims Household Database';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDBListForm, DBListForm);
  Application.Run;
end.

