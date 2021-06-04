unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  Spin, IniFiles, LCLType, zxbthread;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnGo: TButton;
    btnReset: TButton;
    createLoader: TCheckBox;
    zxbPath: TFileNameEdit;
    Label6: TLabel;
    heapSize: TSpinEdit;
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
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure inFileButtonClick(Sender: TObject);
    procedure inFileEditingDone(Sender: TObject);
    procedure outTypeChange(Sender: TObject);
    procedure zxbPathAcceptFileName(Sender: TObject; var Value: String);
    procedure ZXBTerminated(Sender: TObject);
    procedure ZXBOutputAvailable(output: String);
  private
    procedure SetOutputFilename;
    procedure SaveConfig;
    procedure LoadConfig;
  public

  end;

var
  frmMain: TfrmMain;

const
  DEFAULTORG = 32768;
  DEFAULTHEAP = 4768;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.SaveConfig;
var
  ini: TIniFile;
begin
  if not DirectoryExists(GetUserDir + '.zxbui') then mkdir(GetUserDir + '.zxbui');
  ini := TIniFile.Create(GetUserDir + '.zxbui' + PathDelim + 'config.ini');
  ini.WriteString('program','zxbpath',zxbPath.Text);
  ini.WriteInteger('program','output',outType.ItemIndex);
  ini.WriteInteger('program','orgaddr',orgAddress.Value);
  ini.WriteInteger('program','heap',heapSize.Value);
  ini.WriteBool('program','loader',createLoader.Checked);
  ini.WriteBool('program','autorun',doAutorun.Checked);
  ini.WriteString('lastfile','lastinput',inFile.FileName);
  ini.WriteString('lastfile','lastoutput',outFile.FileName);
  ini.Free;
end;

procedure TfrmMain.LoadConfig;
var
  ini: TIniFile;
begin
  if not FileExists(GetUserDir + '.zxbui' + PathDelim + 'config.ini') then exit;
  ini := TIniFile.Create(GetUserDir + '.zxbui' + PathDelim + 'config.ini');
  zxbPath.Text := ini.ReadString('program','zxbpath','');
  outType.ItemIndex := ini.ReadInteger('program','output',0);
  orgAddress.Value := ini.ReadInteger('program','orgaddr',DEFAULTORG);
  heapSize.Value := ini.ReadInteger('program','heap',DEFAULTHEAP);
  createLoader.Checked := ini.ReadBool('program','loader',true);
  doAutorun.Checked := ini.ReadBool('program','autorun',true);
  inFile.Text := ini.ReadString('lastfile','lastinput','');
  outFile.Text := ini.ReadString('lastfile','lastoutput','');
  ini.Free;
end;

procedure TfrmMain.SetOutputFilename;
var
  fparts: TStringArray;
  s: String;
begin
  if inFile.FileName = '' then exit;
  fparts := inFile.Filename.Split('.');
  s := '';
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
  ZXB: TZXBThread;
  Parameters: TStrings;
begin
  Parameters := TStringList.Create;
  btnGo.Enabled := false;

  Parameters.Add('-o');
  Parameters.Add(outFile.FileName);

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
  if orgAddress.Value <> DEFAULTORG then
  begin
    Parameters.Add('-S');
    Parameters.Add(orgAddress.Value.ToString);
  end;
  if heapSize.Value <> DEFAULTHEAP then
  begin
    Parameters.Add('-H');
    Parameters.Add(heapSize.Value.ToString);
  end;
  Parameters.Add(inFile.Filename);
  ZXB := TZXBThread.Create(true);
  ZXB.FreeOnTerminate := true;
  ZXB.SetParams(Parameters);
  ZXB.Executable := zxbPath.Text;
  ZXB.OnTerminate := @ZXBTerminated;
  ZXB.OnOutputAvailable := @ZXBOutputAvailable;
  zxbOutput.Lines.Add('Building '+inFile.FileName);
  ZXB.Start;
end;

procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  if Application.MessageBox('Are you sure you want to reset form elements?',
                            'Confirmation', MB_ICONQUESTION + MB_YESNO) = IDNO
  then exit;
  outType.ItemIndex := 0;
  orgAddress.Value := DEFAULTORG;
  heapSize.Value := DEFAULTHEAP;
  createLoader.Checked := true;
  doAutorun.Checked := true;
  inFile.Text := '';
  outFile.Text := '';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if (zxbPath.Text <> '') and (FileExists(zxbPath.Text)) then
  begin
    zxbOutput.Text := GetZXBASICVersion(zxbPath.Text);
  end;
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

procedure TfrmMain.zxbPathAcceptFileName(Sender: TObject; var Value: String);
begin
  if (Value <> '') and (FileExists(Value)) then
  begin
    zxbOutput.Text := GetZXBASICVersion(Value);
  end;
end;

procedure TfrmMain.ZXBTerminated(Sender: TObject);
begin
  btnGo.Enabled := true;
  zxbOutput.Lines.Add('Done!');
end;

procedure TfrmMain.ZXBOutputAvailable(output: String);
begin
  SaveDebug('UI update! ' + Length(output).ToString + ' bytes');
  zxbOutput.Lines.BeginUpdate;
  zxbOutput.Lines.Text := zxbOutput.Lines.Text + output;
  zxbOutput.Lines.EndUpdate;
  Application.ProcessMessages;
end;

end.

