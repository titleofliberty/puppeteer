unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  Buttons;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    mnuMainViewInventory: TMenuItem;
    mnuMainViewActions: TMenuItem;
    mnuMainViewAbility: TMenuItem;
    mnuMainViewSkills: TMenuItem;
    mnuMainView: TMenuItem;
    mnuMainHelpAbout: TMenuItem;
    mnuMainHelp: TMenuItem;
    mnuMainPuppetsExit: TMenuItem;
    pnlLeft: TScrollBox;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    Separator2: TMenuItem;
    mnuMainPuppetsSaveAs: TMenuItem;
    Separator1: TMenuItem;
    mnuMainPuppetsOpen: TMenuItem;
    mnuMainPuppetsNew: TMenuItem;
    mnuMainPuppets: TMenuItem;
    mnuMain: TMainMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
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

end.

