unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  Buttons, StdCtrls, BCLabel, BCButton;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnActions: TSpeedButton;
    btnTen: TSpeedButton;
    btnTwelve: TSpeedButton;
    btnTwenty: TSpeedButton;
    btnNotes: TSpeedButton;
    btnInventory: TSpeedButton;
    btnEight: TSpeedButton;
    btnWildShapes: TSpeedButton;
    btnSpells: TSpeedButton;
    btnTraits: TSpeedButton;
    btnDescription: TSpeedButton;
    pnlHistory: TPanel;
    pnlClient: TPanel;
    pnlLeft: TScrollBox;
    pnlRight: TScrollBox;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    btnAbilities: TSpeedButton;
    btnSkills: TSpeedButton;
    btnFour: TSpeedButton;
    btnSix: TSpeedButton;
    ScrollBox1: TScrollBox;
    procedure btnFourClick(Sender: TObject);
    procedure mnuMainPuppetsExitClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.mnuMainPuppetsExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnFourClick(Sender: TObject);
begin
  Caption := 'Four';
end;

end.

