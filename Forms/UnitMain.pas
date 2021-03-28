{ *
  * Copyright (C) 2014-2015 ozok <ozok26@gmail.com>
  *
  * This file is part of TEBookConverter.
  *
  * TEBookConverter is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2 of the License, or
  * (at your option) any later version.
  *
  * TEBookConverter is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with TEBookConverter.  If not, see <http://www.gnu.org/licenses/>.
  *
  * }

unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, sListView, sSkinProvider,
  sSkinManager, sStatusBar, Vcl.ExtCtrls, sPanel, Vcl.StdCtrls, sButton,
  sComboBox, Vcl.Mask, sMaskEdit, sCustomComboEdit, sToolEdit, Vcl.Buttons,
  sBitBtn, sDialogs, Vcl.Menus, acPNG, acImage, Vcl.ImgList, acAlphaImageList, UnitEncoder, IniFiles,
  JvComponentBase, JvComputerInfoEx, acProgressBar, JvBaseDlg, JvBrowseFolder, StrUtils,
  JvSearchFiles, sGauge, sCheckBox, ShellAPI, JvDragDrop, System.Types,
  JvThread, JvUrlListGrabber, JvUrlGrabbers, windows7taskbar, sLabel,
  JvCreateProcess, System.ImageList;

type
  TMainForm = class(TForm)
    FileList: TsListView;
    TopBar: TsPanel;
    BottomBar: TsPanel;
    sStatusBar1: TsStatusBar;
    AddBtn: TsBitBtn;
    RemoveBtn: TsBitBtn;
    ClearBtn: TsBitBtn;
    StartBtn: TsBitBtn;
    HelpBtn: TsBitBtn;
    OutEdit: TsDirectoryEdit;
    FormatList: TsComboBox;
    OpenOutBtn: TsButton;
    OpenDialog: TsOpenDialog;
    AddPnl: TsPanel;
    sImage1: TsImage;
    AbortAddBtn: TsButton;
    AddMenu: TPopupMenu;
    A1: TMenuItem;
    A2: TMenuItem;
    A3: TMenuItem;
    DummyList: TsAlphaImageList;
    Info: TJvComputerInfoEx;
    ProcessList: TsComboBox;
    SearchFile: TJvSearchFiles;
    OpenFolder: TJvBrowseForFolderDialog;
    LogBtn: TsBitBtn;
    NormalPnl: TsPanel;
    ConvertPnl: TsPanel;
    ProgressList: TsListView;
    sPanel1: TsPanel;
    OpenOutBtn2: TsBitBtn;
    StopBtn: TsBitBtn;
    LogBtn2: TsBitBtn;
    ProgressImgs: TsAlphaImageList;
    PosTimer: TTimer;
    CreateFolderBtn: TsCheckBox;
    HelpMenu: TPopupMenu;
    A4: TMenuItem;
    C1: TMenuItem;
    H1: TMenuItem;
    N1: TMenuItem;
    C2: TMenuItem;
    InfoBtn: TsBitBtn;
    D1: TMenuItem;
    DragDrop: TJvDragDrop;
    S1: TMenuItem;
    UpdateChecker: TJvHttpUrlGrabber;
    UpdateThread: TJvThread;
    BrandsList: TsComboBox;
    DeviceList: TsComboBox;
    DontIfSameBtn: TsCheckBox;
    ReaderBtn: TsBitBtn;
    ReaderProcess: TJvCreateProcess;
    FileCountLabel: TsLabel;
    DonateBtn: TsBitBtn;
    DonateMenu: TPopupMenu;
    D2: TMenuItem;
    D3: TMenuItem;
    LangBtn: TsBitBtn;
    T1: TMenuItem;
    ProgressBar: TProgressBar;
    ProgressLabel: TLabel;
    CMDGenPnl: TsPanel;
    CreateCMDBar: TsProgressBar;
    procedure FormResize(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure A1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure A3Click(Sender: TObject);
    procedure AbortAddBtnClick(Sender: TObject);
    procedure SearchFileProgress(Sender: TObject);
    procedure SearchFileFindFile(Sender: TObject; const AName: string);
    procedure ClearBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure LogBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure A2Click(Sender: TObject);
    procedure PosTimerTimer(Sender: TObject);
    procedure OpenOutBtnClick(Sender: TObject);
    procedure C1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure A4Click(Sender: TObject);
    procedure H1Click(Sender: TObject);
    procedure C2Click(Sender: TObject);
    procedure InfoBtnClick(Sender: TObject);
    procedure D1Click(Sender: TObject);
    procedure DragDropDrop(Sender: TObject; Pos: TPoint; Value: TStrings);
    procedure S1Click(Sender: TObject);
    procedure UpdateThreadExecute(Sender: TObject; Params: Pointer);
    procedure UpdateCheckerDoneStream(Sender: TObject; Stream: TStream; StreamSize: Integer; Url: string);
    procedure BrandsListChange(Sender: TObject);
    procedure DeviceListChange(Sender: TObject);
    procedure ReaderBtnClick(Sender: TObject);
    procedure DonateBtnClick(Sender: TObject);
    procedure D3Click(Sender: TObject);
    procedure LangBtnClick(Sender: TObject);
    procedure T1Click(Sender: TObject);
  private
    { Private declarations }
    FStopAddFiles: Boolean;
    FCalibrePath: string;
    FReaderPath: string;
    FProcesses: array [0 .. 15] of TEncoder;
    FFiles: TStringList;
    FTotalCMDCount: integer;
    FLastDir: string;
    FFormatsList: TStringList;

    procedure AddFile(const FileName: string);
    function FileSizeEx(const FileName: string): int64;

    procedure CreateCMD(const FileName: string; const ProcessIndex: integer; const FileIndex: integer);

    function GetFileFolderName(const FileName: string): string;

    procedure LoadDevices;
  public
    { Public declarations }
    FAppData: string;
    procedure UpdateProgress;
    procedure SaveToLog(const ProcessID: integer; const MSGs: TStrings; const FileName: string);

    procedure AddToLog(const ID: integer; const MSG: string);

    procedure LoadSettings;
    procedure SaveSettings;
  end;

var
  MainForm: TMainForm;

const
  BuildInt = 260;
  Portable = False;

var
  RS_1: string = '%d files';
  RS_2: string = 'Files %s is empty';
  RS_3: string = 'Unable to add %s';
  RS_4: string = 'Remove all files from the list?';
  RS_5: string = 'Remove All';
  RS_6: string = 'Unable to find/create directory %s';
  RS_7: string = 'Unable to find file %s to create commands.';
  RS_8: string = 'Cannot find a component.';
  RS_9: string = 'Fatal Error';
  RS_10: string = 'Process %d error log:';
  RS_11: string = 'Problematic file is %s';
  RS_12: string = 'Number of processes is greater than cpu/core count.';
  RS_13: string = 'Ignoring %s because it has the same format as the selectd output format';
  RS_14: string = 'Process %d commands:';
  RS_15: string = 'Converting %d files.';
  RS_16: string = 'Using %d threads.';
  RS_17: string = 'Did not create any commands. Perhaps source files no longer exist?';
  RS_18: string = 'Error';
  RS_19: string = 'Info';
  RS_20: string = 'Question';
  RS_21: string = 'Unable to create output folder.';
  RS_22: string = 'Please add files first';
  RS_23: string = 'Stop converting?';
  RS_24: string = 'There is a new version. Would you like to go homepage and download it?';
  RS_25: string = 'All Formats';

implementation

{$R *.dfm}

uses UnitLogs, UnitAbout, UnitInfo, UnitLangs;

procedure TMainForm.A1Click(Sender: TObject);
var
  I: Integer;
begin
  if DirectoryExists(FLastDir) then
  begin
    OpenDialog.InitialDir := FLastDir;
  end;
  if OpenDialog.Execute then
  begin
    FStopAddFiles := False;
    AddPnl.Left := Self.Width div 2 - AddPnl.Width div 2;
    AddPnl.Top := Self.Height div 2 - AddPnl.Height div 2;
    AddPnl.Visible := True;
    AddPnl.BringToFront;
    TopBar.Enabled := False;
    BottomBar.Enabled := False;
    FileList.Enabled := False;
    FileList.Items.BeginUpdate;
    try
      for I := 0 to OpenDialog.Files.Count - 1 do
      begin
        if FStopAddFiles then
        begin
          Break;
        end
        else
        begin
          AddFile(OpenDialog.Files[i]);
        end;
      end;
    finally
      AddPnl.Visible := False;
      TopBar.Enabled := True;
      BottomBar.Enabled := True;
      FileList.Enabled := True;
      FileList.Items.EndUpdate;
      FLastDir := ExtractFileDir(OpenDialog.Files[OpenDialog.Files.Count - 1]);
      FileCountLabel.Caption := Format(RS_1, [FileList.Items.Count]);
    end;
  end;
end;

procedure TMainForm.A2Click(Sender: TObject);
var
  Search: TSearchRec;
  LFileName: String;
  Extension: String;
begin
  if DirectoryExists(FLastDir) then
  begin
    OpenFolder.Directory := FLastDir;
  end;
  if OpenFolder.Execute then
  begin
    FStopAddFiles := False;
    AddPnl.Left := Self.Width div 2 - AddPnl.Width div 2;
    AddPnl.Top := Self.Height div 2 - AddPnl.Height div 2;
    AddPnl.Visible := True;
    AddPnl.BringToFront;
    TopBar.Enabled := False;
    BottomBar.Enabled := False;
    FileList.Enabled := False;
    FileList.Items.BeginUpdate;
    try
      if (FindFirst(OpenFolder.Directory + '\' + '*.*', faAnyFile, Search) = 0) then
      Begin
        repeat
          Application.ProcessMessages;
          if FStopAddFiles then
          begin
            Break;
          end
          else
          begin
            LFileName := OpenFolder.Directory + '\' + Search.Name;
            Extension := LowerCase(ExtractFileExt(LFileName));
            if (Extension = '.cbz') or (Extension = '.cbr') or (Extension = '.cbc') or (Extension = '.chm') or (Extension = '.djvu') or (Extension = '.docx') or (Extension = '.epub') or
              (Extension = '.fb2') or (Extension = '.html') or (Extension = '.htmlz') or (Extension = '.lit') or (Extension = '.lrf') or (Extension = '.mobi') or (Extension = '.odt') or
              (Extension = '.pdf') or (Extension = '.prc') or (Extension = '.pdb') or (Extension = '.pml') or (Extension = '.rb') or (Extension = '.rtf') or (Extension = '.snb') or
              (Extension = '.tcr') or (Extension = '.txt') or (Extension = '.txtz') then
            begin
              AddFile(LFileName);
            end;
          end;
        until (FindNext(Search) <> 0) and (not FStopAddFiles);
        FindClose(Search);
      end;
    finally
      AddPnl.Visible := False;
      TopBar.Enabled := True;
      BottomBar.Enabled := True;
      FileList.Enabled := True;
      FileList.Items.EndUpdate;
      FLastDir := OpenFolder.Directory;
      FileCountLabel.Caption := Format(RS_1, [FileList.Items.Count]);
    end;
  end;
end;

procedure TMainForm.A3Click(Sender: TObject);
begin
  if DirectoryExists(FLastDir) then
  begin
    OpenFolder.Directory := FLastDir;
  end;
  if OpenFolder.Execute then
  begin
    FStopAddFiles := False;
    AddPnl.Left := Self.Width div 2 - AddPnl.Width div 2;
    AddPnl.Top := Self.Height div 2 - AddPnl.Height div 2;
    AddPnl.Visible := True;
    AddPnl.BringToFront;
    TopBar.Enabled := False;
    BottomBar.Enabled := False;
    FileList.Enabled := False;
    FileList.Items.BeginUpdate;
    try
      SearchFile.RecurseDepth := maxint;
      SearchFile.RootDirectory := OpenFolder.Directory;
      SearchFile.Search;
    finally
      AddPnl.Visible := False;
      TopBar.Enabled := True;
      BottomBar.Enabled := True;
      FileList.Enabled := True;
      FileList.Items.EndUpdate;
      FLastDir := OpenFolder.Directory;
      FileCountLabel.Caption := Format(RS_1, [FileList.Items.Count]);
    end;
  end;
end;

procedure TMainForm.A4Click(Sender: TObject);
begin
  Self.Enabled := False;
  AboutForm.Show;
end;

procedure TMainForm.AbortAddBtnClick(Sender: TObject);
begin
  if SearchFile.Searching then
  begin
    SearchFile.Abort;
  end
  else
  begin
    FStopAddFiles := True;
  end;
end;

procedure TMainForm.AddBtnClick(Sender: TObject);
var
  P: TPoint;
begin
  P := AddBtn.ClientToScreen(Point(0, 0));
  AddMenu.Popup(P.X, P.Y + AddBtn.Height)
end;

procedure TMainForm.AddFile(const FileName: string);
var
  LItem: TListItem;
  LSize: int64;
begin
  if FileExists(FileName) then
  begin
    LSize := FileSizeEx(FileName);
    if LSize > 0 then
    begin
      LItem := FileList.Items.Add;
      LItem.Caption := ExtractFileName(FileName);
      LItem.SubItems.Add(FormatFloat('0.###', FileSizeEx(FileName) / 1024) + ' KB');
      LItem.SubItems.Add(Copy(UpperCase(ExtractFileExt(FileName)), 2, maxint));
      LItem.Checked := True;
      FFiles.Add(FileName);
    end
    else
    begin
      AddToLog(1, Format(RS_2, [FileName]));
    end;
  end
  else
  begin
    AddToLog(1, Format(RS_3, [FileName]));
  end;
end;

procedure TMainForm.AddToLog(const ID: integer; const MSG: string);
begin
  case ID of
    0:
      begin
        if Length(MSG) > 0 then
        begin
          LogForm.MainList.Lines.Add('[' + DateTimeToStr(Now) + '] ' + MSG)
        end
        else
        begin
          LogForm.MainList.Lines.Add('')
        end;
      end;
    1:
      begin
        if Length(MSG) > 0 then
        begin
          LogForm.FileAddList.Lines.Add('[' + DateTimeToStr(Now) + '] ' + MSG)
        end
        else
        begin
          LogForm.FileAddList.Lines.Add('')
        end;
      end;
    2:
      begin
        LogForm.CMDList.Lines.Add(MSG)
      end;
    3:
      begin
        if Length(MSG) > 0 then
        begin
          LogForm.CalibreList.Lines.Add('[' + DateTimeToStr(Now) + '] ' + MSG)
        end
        else
        begin
          LogForm.CalibreList.Lines.Add('')
        end;
      end;
  end;
end;

procedure TMainForm.BrandsListChange(Sender: TObject);
begin
  LoadDevices;
end;

procedure TMainForm.C1Click(Sender: TObject);
begin
  if FileExists(ExtractFileDir(Application.ExeName) + '\changelog.txt') then
  begin
    ShellExecute(handle, 'open', PWideChar(ExtractFileDir(Application.ExeName) + '\changelog.txt'), nil, nil, SW_NORMAL);
  end;
end;

procedure TMainForm.C2Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'http://calibre-ebook.com/', nil, nil, SW_NORMAL);
end;

procedure TMainForm.ClearBtnClick(Sender: TObject);
begin
  if FileList.Items.Count = 0 then
    Exit;

  if ID_YES = Application.MessageBox(PWideChar(RS_4), PWideChar(RS_5), MB_ICONQUESTION or MB_YESNO) then
  begin
    FileList.Items.Clear;
    FFiles.Clear;
  end;
  FileCountLabel.Caption := Format(RS_1, [FileList.Items.Count]);
end;

procedure TMainForm.CreateCMD(const FileName: string; const ProcessIndex: integer; const FileIndex: integer);
var
  LOutName: string;
begin
  if FileExists(FileName) then
  begin
    FProcesses[ProcessIndex].EncoderPaths.Add(FCalibrePath);
    LOutName := OutEdit.Text + '\' + GetFileFolderName(FileName) + ChangeFileExt(ExtractFileName(FileName), '.' + LowerCase(FormatList.Text));
    FProcesses[ProcessIndex].CommandLines.Add(' "' + FileName + '" "' + LOutName + '" --dont-grayscale');
    FProcesses[ProcessIndex].FileIndexes.Add(FileIndex);
    FProcesses[ProcessIndex].OutputFolder.Add(GetFileFolderName(FileName) + (ExtractFileName(FileName)));
  end
  else
  begin
    AddToLog(0, Format(RS_7, [FileName]));
  end;
end;

procedure TMainForm.D1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=7WNE4J2AQWQJ8', nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.D3Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://calibre-ebook.com/donate', nil, nil, SW_SHOWNORMAL)
end;

procedure TMainForm.DeviceListChange(Sender: TObject);
begin
  if DeviceList.ItemIndex > -1 then
  begin
    FormatList.Items.DelimitedText := FFormatsList[DeviceList.ItemIndex];
    if FormatList.Items.Count > 0 then
    begin
      FormatList.ItemIndex := 0;
    end;
  end;
end;

procedure TMainForm.DonateBtnClick(Sender: TObject);
var
  P: TPoint;
begin
  P := DonateBtn.ClientToScreen(Point(0, 0));
  DonateMenu.Popup(P.X, P.Y + DonateBtn.Height)
end;

procedure TMainForm.DragDropDrop(Sender: TObject; Pos: TPoint; Value: TStrings);
var
  i: Integer;
  Extension: string;
  DirectoriesToSearch: TStringList;
begin
  FStopAddFiles := False;
  AddPnl.Left := Self.Width div 2 - AddPnl.Width div 2;
  AddPnl.Top := Self.Height div 2 - AddPnl.Height div 2;
  AddPnl.Visible := True;
  AddPnl.BringToFront;
  TopBar.Enabled := False;
  BottomBar.Enabled := False;
  FileList.Enabled := False;
  FileList.Items.BeginUpdate;
  DirectoriesToSearch := TStringList.Create;
  try
    for i := 0 to Value.Count - 1 do
    begin
      Application.ProcessMessages;
      Extension := LowerCase(ExtractFileExt(Value[i]));
      if FStopAddFiles then
      begin
        Break;
      end
      else
      begin
        // decide if file or directory
        if DirectoryExists(Value[i]) then
        begin
          DirectoriesToSearch.Add(Value[i]);
        end
        else
        begin
          if (Extension = '.cbz') or (Extension = '.cbr') or (Extension = '.cbc') or (Extension = '.chm') or (Extension = '.djvu') or (Extension = '.docx') or (Extension = '.epub') or
            (Extension = '.fb2') or (Extension = '.html') or (Extension = '.htmlz') or (Extension = '.lit') or (Extension = '.lrf') or (Extension = '.mobi') or (Extension = '.odt') or
            (Extension = '.pdf') or (Extension = '.prc') or (Extension = '.pdb') or (Extension = '.pml') or (Extension = '.rb') or (Extension = '.rtf') or (Extension = '.snb') or
            (Extension = '.tcr') or (Extension = '.txt') or (Extension = '.txtz') then
          begin
            AddFile(Value[i]);
            FLastDir := ExtractFileDir(Value[i]);
          end;
        end;
      end;
    end;
    // add directory content
    if not FStopAddFiles then
    begin
      if DirectoriesToSearch.Count > 0 then
      begin
        for I := 0 to DirectoriesToSearch.Count - 1 do
        begin
          Application.ProcessMessages;
          SearchFile.RootDirectory := DirectoriesToSearch[i];
          SearchFile.Search;
          FLastDir := DirectoriesToSearch[i];
        end;
      end;
    end;
  finally
    AddPnl.Visible := False;
    TopBar.Enabled := True;
    BottomBar.Enabled := True;
    FileList.Enabled := True;
    FileList.Items.EndUpdate;
    DirectoriesToSearch.Free;
    FileCountLabel.Caption := Format(RS_1, [FileList.Items.Count]);
  end;
end;

function TMainForm.FileSizeEx(const FileName: string): int64;
var
  LFS: TFileStream;
begin
  LFS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LFS.Size;
  finally
    LFS.Free;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
begin
  SaveSettings;
  for I := Low(FProcesses) to High(FProcesses) do
  begin
    if FProcesses[i].CommandCount > 0 then
    begin
      FProcesses[i].Stop;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  if not Portable then
  begin
    FAppData := Info.Folders.AppData + '\TEBookConverter';
  end
  else
  begin
    FAppData := ExtractFileDir(Application.ExeName);
    sStatusBar1.Panels[0].Text := sStatusBar1.Panels[0].Text + ' Portable';
  end;
  if not DirectoryExists(FAppData) then
  begin
    ForceDirectories(FAppData)
  end;
  for I := Low(FProcesses) to High(FProcesses) do
  begin
    FProcesses[i] := TEncoder.Create;
    FProcesses[i].ID := i;
  end;
  FFiles := TStringList.Create;
  FFormatsList := TStringList.Create;

  FCalibrePath := ExtractFileDir(Application.ExeName) + '\Calibre\ebook-convert.exe';
  FReaderPath := ExtractFileDir(Application.ExeName) + '\Calibre\ebook-viewer.exe';
  if not FileExists(FCalibrePath) then
  begin
    Application.MessageBox(PWideChar(RS_8), PWideChar(RS_9), MB_ICONERROR);
    Application.Terminate;
  end;
  if not FileExists(FReaderPath) then
  begin
    Application.MessageBox(PWideChar(RS_8), PWideChar(RS_9), MB_ICONERROR);
    Application.Terminate;
  end
  else
  begin
    ReaderProcess.ApplicationName := FReaderPath;
  end;
  // windows 7 taskbar
  if CheckWin32Version(6, 1) then
  begin
    if not InitializeTaskbarAPI then
    begin
      Application.MessageBox('You seem to have Windows 7 but program can''t start taskbar progressbar!', 'Error', MB_ICONERROR);
    end;
  end;

  FormatList.Items.StrictDelimiter := True;
  FormatList.Items.Delimiter := ',';
  LoadSettings;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := Low(FProcesses) to High(FProcesses) do
  begin
    FProcesses[i].Free;
  end;
  FFiles.Free;
  FFormatsList.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  FileList.Columns[0].Width := FileList.ClientWidth - FileList.Columns[1].Width - FileList.Columns[2].Width - 20;
  ProgressList.Columns[0].Width := FileList.ClientWidth - FileList.Columns[1].Width - 20;
  sStatusBar1.Panels[1].Width := sStatusBar1.ClientWidth - sStatusBar1.Panels[2].Width - sStatusBar1.Panels[0].Width - 80;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateThread.Execute(nil);
end;

function TMainForm.GetFileFolderName(const FileName: string): string;
var
  TmpStr: string;
  i: integer;
  FolderName: string;
begin
  Result := '';
  if not CreateFolderBtn.Checked then
    Exit;

  if FileExists(FileName) then
  begin
    TmpStr := ReverseString(ExtractFileDir(FileName));
    if TmpStr[1] = '\' then
    begin
      Delete(TmpStr, 1, 1);
    end;
    FolderName := '';
    for I := 1 to Length(TmpStr) do
    begin
      if TmpStr[i] <> '\' then
      begin
        FolderName := FolderName + TmpStr[i];
      end
      else
      begin
        Break;
      end;
    end;
    if Length(FolderName) > 0 then
    begin
      Result := ReverseString(FolderName) + '\';
      if Copy(Result, 2, 1) = ':' then
      begin
        Result := '';
      end;
    end;
  end;
end;

procedure TMainForm.H1Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://sourceforge.net/projects/tebookconverter/', nil, nil, SW_NORMAL);
end;

procedure TMainForm.HelpBtnClick(Sender: TObject);
var
  P: TPoint;
begin
  P := HelpBtn.ClientToScreen(Point(0, 0));
  HelpMenu.Popup(P.X, P.Y + HelpBtn.Height);
end;

procedure TMainForm.InfoBtnClick(Sender: TObject);
begin
  if FileList.ItemIndex > -1 then
  begin
    FileInfoForm.FileName := FFiles[FileList.ItemIndex];
    Self.Enabled := False;
    FileInfoForm.Show;
  end;
end;

procedure TMainForm.LangBtnClick(Sender: TObject);
begin
  Self.Enabled := False;
  LangForm.Show;
end;

procedure TMainForm.LoadDevices;
var
  LProfileFile: TStringList;
  LDeviceName: string;
  LFormats: string;
  I: Integer;
  LLine: String;
  LPos1: integer;
begin
  if BrandsList.ItemIndex > -1 then
  begin
    DeviceList.Items.Clear;
    FormatList.Items.Clear;
    FFormatsList.Clear;
    LProfileFile := TStringList.Create;
    try
      // all is different in all languages so it gets different treatment
      if BrandsList.ItemIndex = 0 then
      begin
        if FileExists(ExtractFileDir(Application.ExeName) + '\Presets\All.txt') then
        begin
          LProfileFile.LoadFromFile(ExtractFileDir(Application.ExeName) + '\Presets\All.txt');
          DeviceList.Items.Add(RS_25);
          LFormats := LProfileFile[0];
          FFormatsList.Add(LFormats);
        end;
      end
      else
      begin
        if FileExists(ExtractFileDir(Application.ExeName) + '\Presets\' + BrandsList.Text + '.txt') then
        begin
          LProfileFile.LoadFromFile(ExtractFileDir(Application.ExeName) + '\Presets\' + BrandsList.Text + '.txt');
          for I := 0 to LProfileFile.Count - 1 do
          begin
            LLine := Trim(LProfileFile[i]);
            LPos1 := Pos(';', LLine);
            if LPos1 > 0 then
            begin
              LDeviceName := Copy(LLine, 1, LPos1 - 1);
              LFormats := UpperCase(Copy(LLine, LPos1 + 1, MaxInt));
              FFormatsList.Add(LFormats);
              DeviceList.Items.Add(LDeviceName);
            end;
          end;
        end;
      end;
    finally
      LProfileFile.Free;
      if DeviceList.Items.Count > 0 then
      begin
        DeviceList.ItemIndex := 0;
        FormatList.Items.DelimitedText := FFormatsList[0];
        if FormatList.Items.Count > 0 then
        begin
          FormatList.ItemIndex := 0;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.LoadSettings;
var
  LSF: TIniFile;
begin
  LSF := TIniFile.Create(FAppData + '\settings.ini');
  try
    with LSF do
    begin
      if not Portable then
      begin
        OutEdit.Text := ReadString('settings', 'out', Info.Folders.Personal + '\TEBookConverter');
      end
      else
      begin
        OutEdit.Text := ReadString('settings', 'out', ExtractFileDir(Application.ExeName));
      end;

      BrandsList.ItemIndex := ReadInteger('settings', 'brand', 0);
      BrandsListChange(Self);
      DeviceList.ItemIndex := ReadInteger('settings', 'device', 0);
      DeviceListChange(Self);
      FormatList.ItemIndex := ReadInteger('settings', 'format', 0);
      if CPUCount > 16 then
      begin
        ProcessList.ItemIndex := ReadInteger('settings', 'process', 16);
      end
      else
      begin
        ProcessList.ItemIndex := ReadInteger('settings', 'process', CPUCount - 1);
      end;
      FLastDir := ReadString('settings', 'lastdir', Info.Folders.Personal);
      CreateFolderBtn.Checked := ReadBool('settings', 'folder', True);
      DontIfSameBtn.Checked := ReadBool('settings', 'SameFormat', True);
    end;
  finally
    if not DirectoryExists(OutEdit.Text) then
    begin
      ForceDirectories(OutEdit.Text)
    end;
    LSF.Free;
  end;
end;

procedure TMainForm.LogBtnClick(Sender: TObject);
begin
  LogForm.Show;
end;

procedure TMainForm.OpenOutBtnClick(Sender: TObject);
begin
  if DirectoryExists(OutEdit.Text) then
  begin
    ShellExecute(handle, 'open', PWideChar(OutEdit.Text), nil, nil, SW_NORMAL);
  end;
end;

procedure TMainForm.PosTimerTimer(Sender: TObject);
var
  LDoneCount: integer;
  I: Integer;
begin
  LDoneCount := 0;
  for I := Low(FProcesses) to High(FProcesses) do
  begin
    if FProcesses[i].CommandCount > 0 then
    begin
      Inc(LDoneCount, FProcesses[i].FilesDone);
    end;
  end;
  // if finished all
  if LDoneCount = FTotalCMDCount then
  begin
    PosTimer.Enabled := False;
    ConvertPnl.Visible := False;
    SetProgressValue(handle, 0, maxint);
    AddToLog(0, 'Finished converting.');
    AddToLog(0, '==END==');
    AddToLog(0, '');
    Self.Caption := 'TEBookConverter';
  end;
end;

procedure TMainForm.ReaderBtnClick(Sender: TObject);
begin
  if FileList.ItemIndex > -1 then
  begin
    ShellExecute(handle, 'open', PWideChar(FReaderPath), PWideChar('"' + FFiles[FileList.ItemIndex] + '"'), nil, SW_SHOWNORMAL);
  end;
end;

procedure TMainForm.RemoveBtnClick(Sender: TObject);
var
  I: Integer;
begin
  if FileList.Items.Count < 1 then
    Exit;

  for I := FileList.Items.Count - 1 downto 0 do
  begin
    if FileList.Items[i].Selected then
    begin
      FileList.Items.Delete(i);
      FFiles.Delete(i);
    end;
  end;
  FileCountLabel.Caption := Format(RS_1, [FileList.Items.Count]);
end;

procedure TMainForm.S1Click(Sender: TObject);
const
  NewLine = '%0D%0A';
var
  mail: PChar;
  mailbody: string;
begin
  mailbody := AboutForm.sLabel1.Caption;
  mailbody := mailbody + NewLine + 'Bugs: ' + NewLine + NewLine + NewLine + 'Suggestions: ' + NewLine + NewLine + NewLine;
  mail := PWideChar('mailto:ozok26@gmail.com?subject=TEBookConverter&body=' + mailbody);

  ShellExecute(0, 'open', mail, nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.SaveSettings;
var
  LSF: TIniFile;
begin
  LSF := TIniFile.Create(FAppData + '\settings.ini');
  try
    with LSF do
    begin
      WriteString('settings', 'out', OutEdit.Text);
      WriteInteger('settings', 'format', FormatList.ItemIndex);
      WriteInteger('settings', 'process', ProcessList.ItemIndex);
      WriteString('settings', 'lastdir', FLastDir);
      WriteBool('settings', 'folder', CreateFolderBtn.Checked);
      WriteInteger('settings', 'device', DeviceList.ItemIndex);
      WriteInteger('settings', 'brand', BrandsList.ItemIndex);
      WriteBool('settings', 'SameFormat', DontIfSameBtn.Checked);
    end;
  finally
    if Length(OutEdit.Text) > 0 then
    begin
      if not DirectoryExists(OutEdit.Text) then
      begin
        try
          ForceDirectories(OutEdit.Text)
        except
          on E: EInOutError do
        end;
      end;
    end;
    LSF.Free;
  end;
end;

procedure TMainForm.SaveToLog(const ProcessID: integer; const MSGs: TStrings; const FileName: string);
begin
  LogForm.CalibreList.Lines.Add(Format(RS_10, [ProcessID]));
  LogForm.CalibreList.Lines.Add(Format(RS_11, [FileName]));
  LogForm.CalibreList.Lines.AddStrings(MSGs);
end;

procedure TMainForm.SearchFileFindFile(Sender: TObject; const AName: string);
begin
  AddFile(AName);
end;

procedure TMainForm.SearchFileProgress(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TMainForm.StartBtnClick(Sender: TObject);
var
  LProcessCount: integer;
  I: Integer;
  J: Integer;
begin
  if FileList.Items.Count > 0 then
  begin
    if not DirectoryExists(OutEdit.Text) then
    begin
      ForceDirectories(OutEdit.Text)
    end;
    if DirectoryExists(OutEdit.Text) then
    begin
      // reset
      for I := Low(FProcesses) to High(FProcesses) do
      begin
        FProcesses[i].ResetValues;
      end;
      FTotalCMDCount := 0;
      ProgressList.Items.Clear;
      ProgressBar.Position := 0;
      LogForm.CMDList.Lines.Clear;
      LogForm.CalibreList.Lines.Clear;
      // process count
      LProcessCount := ProcessList.ItemIndex + 1;
      if LProcessCount > CPUCount then
      begin
        AddToLog(0, RS_12);
      end;
      CreateCMDBar.Position := 0;
      CreateCMDBar.Max := FileList.Items.Count;
      CMDGenPnl.Left := Self.Width div 2 - CMDGenPnl.Width div 2;
      CMDGenPnl.Top := Self.Height div 2 - CMDGenPnl.Height div 2;
      CMDGenPnl.Visible := True;
      CMDGenPnl.BringToFront;
      try
        for I := 0 to FFiles.Count - 1 do
        begin
          Application.ProcessMessages;
          CreateCMDBar.Position := i + 1;

          if FileList.Items[i].Checked then
          begin
            if DontIfSameBtn.Checked then
            begin
              if ('.' + LowerCase(FormatList.Text)) = LowerCase(ExtractFileExt(FFiles[i])) then
              begin
                AddToLog(0, Format(RS_13, [ExtractFileName(FFiles[i])]));
                Continue;
              end
              else
              begin
                CreateCMD(FFiles[i], i mod LProcessCount, i);
              end;
            end
            else
            begin
              CreateCMD(FFiles[i], i mod LProcessCount, i);
            end;
          end;
        end;
        // log command lines
        for I := Low(FProcesses) to High(FProcesses) do
        begin
          if FProcesses[i].CommandCount > 0 then
          begin
            AddToLog(2, Format(RS_14, [i + 1]));
            for J := 0 to FProcesses[i].CommandCount - 1 do
            begin
              AddToLog(2, '   ' + FProcesses[i].CommandLines[J]);
            end;
          end;
        end;
        AddToLog(2, '');
        // launch the processes
        for I := Low(FProcesses) to High(FProcesses) do
        begin
          if FProcesses[i].CommandCount > 0 then
          begin
            Inc(FTotalCMDCount, FProcesses[i].CommandCount);
            FProcesses[i].Start;
          end;
        end;
      finally
        CMDGenPnl.Visible := False;
        CMDGenPnl.SendToBack;
      end;

      if FTotalCMDCount > 0 then
      begin
        ProgressBar.Max := FTotalCMDCount;
        ProgressLabel.Caption := '0/' + FloatToStr(FTotalCMDCount);
        AddToLog(0, Format(RS_15, [FTotalCMDCount]));
        AddToLog(0, Format(RS_16, [LProcessCount]));
        AddToLog(0, '');
        PosTimer.Enabled := True;
        SetProgressState(handle, tbpsNormal);
        Self.Caption := '[0 %] TEBookConverter';
        ConvertPnl.Visible := True;
        ConvertPnl.BringToFront;
      end
      else
      begin
        Application.MessageBox(PWideChar(RS_17), PWideChar(RS_19), MB_ICONINFORMATION);
      end;
    end
    else
    begin
      Application.MessageBox(PWideChar(RS_21), PWideChar(RS_18), MB_ICONERROR)
    end;
  end
  else
  begin
    Application.MessageBox(PWideChar(RS_22), PWideChar(RS_18), MB_ICONERROR)
  end;
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
var
  I: Integer;
begin
  if ID_YES = Application.MessageBox(PWideChar(RS_23), PWideChar(RS_20), MB_ICONQUESTION or MB_YESNO) then
  begin
    for I := Low(FProcesses) to High(FProcesses) do
    begin
      if FProcesses[i].CommandCount > 0 then
      begin
        FProcesses[i].Stop;
      end;
    end;
    ConvertPnl.Visible := False;
    PosTimer.Enabled := False;
    SetProgressValue(handle, 0, maxint);
    AddToLog(0, 'Stopped by user.');
    AddToLog(0, '==END==');
    AddToLog(0, '');
    Self.Caption := 'TEBookConverter';
  end;
end;

procedure TMainForm.T1Click(Sender: TObject);
begin
  if FileExists(ExtractFileDir(Application.ExeName) + '\translate.txt') then
  begin
    ShellExecute(handle, 'open', PWideChar(ExtractFileDir(Application.ExeName) + '\translate.txt'), nil, nil, SW_NORMAL);
  end;
end;

procedure TMainForm.UpdateCheckerDoneStream(Sender: TObject; Stream: TStream; StreamSize: Integer; Url: string);
var
  VersionFile: TStringList;
  LatestVersion: Integer;
begin
  VersionFile := TStringList.Create;
  try
    if StreamSize > 0 then
    begin
      VersionFile.LoadFromStream(Stream);
      if VersionFile.Count = 1 then
      begin
        if TryStrToInt(VersionFile.Strings[0], LatestVersion) then
        begin
          if LatestVersion > BuildInt then
          begin
            if ID_YES = Application.MessageBox(PWideChar(RS_23), PWideChar(RS_20), MB_ICONQUESTION or MB_YESNO) then
            begin
              ShellExecute(handle, 'open', 'https://sourceforge.net/projects/tebookconverter/', nil, nil, SW_NORMAL);
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(VersionFile);
  end;
end;

procedure TMainForm.UpdateProgress;
begin
  ProgressBar.Position := ProgressBar.Position + 1;
  ProgressLabel.Caption := FloatToStr(ProgressBar.Position) + '/' + FloatToStr(FTotalCMDCount);
  SetProgressValue(handle, ProgressBar.Position, ProgressBar.Max);
  Self.Caption := '[' + FloatToStr((100 * ProgressBar.Position) div ProgressBar.Max) + '%] TEBookConverter'
end;

procedure TMainForm.UpdateThreadExecute(Sender: TObject; Params: Pointer);
begin
  with UpdateChecker do
  begin
    Url := 'http://sourceforge.net/projects/tebookconverter/files/version.txt/download';
    Start;
  end;

  UpdateThread.CancelExecute;
end;

end.
