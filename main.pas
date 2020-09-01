unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  Spin, Process, DateUtils, Math;

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
  AProcess     : TProcess;
  CharBuffer: array [0..511] of char;
  ReadCount: integer;
  zxbex: String;
  s: String;
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

  AProcess := TProcess.Create(nil);
  AProcess.Executable := zxbex;

  if (createLoader.Checked) and (createLoader.Enabled) then
  begin
    AProcess.Parameters.Add('-B');
  end;
  if (doAutorun.Checked) and (doAutorun.Enabled) then
  begin
    AProcess.Parameters.Add('-a');
  end;
  case outType.ItemIndex of
    0: begin
         AProcess.Parameters.Add('-t');
       end;
    1: begin
         AProcess.Parameters.Add('-T');
       end;
    2: begin
         AProcess.Parameters.Add('-A');
       end;
  end;
  AProcess.Parameters.Add(inFile.Filename);

  RunCommand(zxbex,['--version'],s);
  zxbOutput.Lines.Add(s);

  btnGo.Enabled := false;
  AProcess.Options := [poUsePipes, poStdErrToOutPut];
  AProcess.ShowWindow := swoHide;
  AProcess.Execute;

  while AProcess.Running do
  begin
    {while AProcess.Stderr.NumBytesAvailable > 0 do
    begin
      ReadCount := Min(512, AProcess.Stderr.NumBytesAvailable); //Read up to buffer, not more
      AProcess.Stderr.Read(CharBuffer, ReadCount);
      zxbOutput.Lines.Append(Copy(CharBuffer, 0, ReadCount));
    end;}
    while AProcess.Output.NumBytesAvailable > 0 do
    begin
      ReadCount := Min(512, AProcess.Output.NumBytesAvailable); //Read up to buffer, not more
      AProcess.Output.Read(CharBuffer, ReadCount);
      zxbOutput.Lines.Append(Copy(CharBuffer, 0, ReadCount));
    end;
    Application.ProcessMessages;
  end;
  btnGo.Enabled := true;
  AProcess.Free;
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

end.

