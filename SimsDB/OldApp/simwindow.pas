unit simwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, simdata;

type

  { TSimForm }

  TSimForm = class(TForm)
    BtnSave: TButton;
    BtnClose: TButton;
    Sim1: TSim;
  private

  public

  end;

implementation

{$R *.lfm}

{ TSimForm }

end.

