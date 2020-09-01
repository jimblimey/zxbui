unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  Spin, zxbthread;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnGo: TButton;
    createLoader: TCheckBox;
    zxbPath: TDirectoryEdit;
    doAutorun: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    outType: TComboBox;
    inFile: TFileNameEdit;
    outFile: TFileNameEdit;
    zxbOutput: TMemo;
    orgAddress: TSpinEdit;
    procedure btnGoClick(Sender: TObject);
    procedure inFileButtonClick(Sender: TObject);
    procedure inFileEditingDone(Sender: TObject);
    procedure outTypeChange(Sender: TObject);
    procedure ZXBTerminated(Sender: TObject);
    procedure ZXBOutputAvailable(output: String);
  private
    procedure SetOutputFilename;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.SetOutputFilename;
var
  fparts: TStringArray;
  s: String;
begin
  fparts := inFile.Filename.Split('.');
  if Length(fparts) > 0 then
  begin
    case outType.ItemIndex of
      0: fparts[High(fparts)] := 'tap';
      1: fparts[High(fparts)] := 'tzx';
      2: fparts[High(fparts)] := 'asm';
      3: fparts[High(fparts)] := 'bin';
    end;
    s := s.Join('.',fparts);
    outFile.Filename := s;
  end;
end;

procedure TfrmMain.btnGoClick(Sender: TObject);
var
  zxbex: String;
  s: String;
  ZXB: TZXBThread;
  Parameters: TStrings;
begin
  zxbex := '';
  {$IFDEF WINDOWS}
    if FileExists(IncludeTrailingPathDelimiter(zxbPath.Directory) + 'zxbc.exe') then
      zxbex := IncludeTrailingPathDelimiter(zxbPath.Directory) + 'zxbc.exe'
    else if FileExists(IncludeTrailingPathDelimiter(zxbPath.Directory) + 'zxb.exe') then
      zxbex := IncludeTrailingPathDelimiter(zxbPath.Directory) + 'zxb.exe';
  {$ELSE}
      if FileExists(IncludeTrailingPathDelimiter(zxbPath.Directory) + 'zxbc.py') then
        zxbex := IncludeTrailingPathDelimiter(zxbPath.Directory) + 'zxbc.py'
      else if FileExists(IncludeTrailingPathDelimiter(zxbPath.Directory) + 'zxb.py') then
        zxbex := IncludeTrailingPathDelimiter(zxbPath.Directory) + 'zxb.py';
  {$ENDIF}

  btnGo.Enabled := false;
  Parameters := TStringList.Create;
  if (createLoader.Checked) and (createLoader.Enabled) then
  begin
    Parameters.Add('-B');
  end;
  if (doAutorun.Checked) and (doAutorun.Enabled) then
  begin
    Parameters.Add('-a');
  end;
  case outType.ItemIndex of
    0: begin
         Parameters.Add('-t');
       end;
    1: begin
         Parameters.Add('-T');
       end;
    2: begin
         Parameters.Add('-A');
       end;
  end;
  Parameters.Add(inFile.Filename);
  ZXB := TZXBThread.Create(true);
  ZXB.FreeOnTerminate := true;
  ZXB.SetParams(Parameters);
  ZXB.Executable := zxbex;
  ZXB.OnTerminate := @ZXBTerminated;
  ZXB.OnOutputAvailable := @ZXBOutputAvailable;
  ZXB.Start;
end;

procedure TfrmMain.inFileButtonClick(Sender: TObject);
begin
  SetOutputFilename;
end;

procedure TfrmMain.inFileEditingDone(Sender: TObject);
begin
  SetOutputFilename;
end;

procedure TfrmMain.outTypeChange(Sender: TObject);
begin
  SetOutputFilename;
end;

procedure TfrmMain.ZXBTerminated(Sender: TObject);
begin
  btnGo.Enabled := true;
end;

procedure TfrmMain.ZXBOutputAvailable(output: String);
begin
  zxbOutput.Lines.Text := zxbOutput.Lines.Text + output;
  Application.ProcessMessages;
end;

end.

