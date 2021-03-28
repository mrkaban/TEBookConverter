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

unit UnitLangs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, acAlphaImageList,
  Vcl.StdCtrls, Vcl.ComCtrls, sComboBoxes, sSkinProvider, IniFiles,
  System.ImageList;

type
  TLangForm = class(TForm)
    LangsList: TsComboBoxEx;
    LagImgs: TsAlphaImageList;
    procedure LangsListChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure SaveLoadLang(const Saving: Boolean);
  public
    { Public declarations }
  end;

var
  LangForm: TLangForm;

implementation

{$R *.dfm}

uses UnitMain, UnitAbout, UnitInfo, UnitLogs;

procedure TLangForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveLoadLang(True);
  MainForm.Enabled := True;
  MainForm.BringToFront;
end;

procedure TLangForm.FormCreate(Sender: TObject);
begin
  SaveLoadLang(False);
end;

procedure TLangForm.LangsListChange(Sender: TObject);
var
  LLangFile: TMemIniFile;
  LLangFilePath: string;
begin
  case LangsList.ItemIndex of
    0:
      LLangFilePath := 'TEBookConverter.tr.lng';
    1:
      LLangFilePath := 'TEBookConverter.lng';
    2:
      LLangFilePath := 'TEBookConverter.fr.lng';
    3:
      LLangFilePath := 'TEBookConverter.ru.lng';
    4:
      LLangFilePath := 'TEBookConverter.pt.lng';
    5:
      LLangFilePath := 'TEBookConverter.es.lng';
    6:
      LLangFilePath := 'TEBookConverter.hu.lng';
    7:
      LLangFilePath := 'TEBookConverter.de.lng';
    8:
      LLangFilePath := 'TEBookConverter.el.lng';
  end;
  LLangFile := TMemIniFile.Create(ExtractFileDir(Application.ExeName) + '\' + LLangFilePath, TEncoding.UTF8);
  try
    with LLangFile do
    begin
      // aboutform
      with AboutForm do
      begin
        Caption := ReadString('TAboutForm', 'Caption', 'About');
        sLabel1.Caption := ReadString('TAboutForm', 'sLabel1.Caption', 'TEBookConverter');
        sLabel2.Caption := ReadString('TAboutForm', 'sLabel2.Caption', '2014 (C) ozok - ozok26@gmail.com GPL3');
        sLabel3.Caption := ReadString('TAboutForm', 'sLabel3.Caption', 'Uses Calibre');
        sButton1.Caption := ReadString('TAboutForm', 'sButton1.Caption', 'Homepage');
        sButton2.Caption := ReadString('TAboutForm', 'sButton2.Caption', 'Close');
        sListView1.Columns[0].Caption := ReadString('TAboutForm', 'sListView1.Columns.(0).Caption', 'Language');
        sListView1.Columns[1].Caption := ReadString('TAboutForm', 'sListView1.Columns.(1).Caption', 'Translator');
        sListView1.Columns[2].Caption := ReadString('TAboutForm', 'sListView1.Columns.(2).Caption', 'E-Mail');
      end;
      // TFileInfoForm
      with FileInfoForm do
      begin
        Caption := ReadString('TFileInfoForm', 'Caption', 'File Information');
        InfoList.Columns[0].Caption := ReadString('TFileInfoForm', 'InfoList.Columns.(0).Caption', 'Key');
        InfoList.Columns[1].Caption := ReadString('TFileInfoForm', 'InfoList.Columns.(1).Caption', 'Value');
      end;
      // TLangForm
      with LangForm do
      begin
        Caption := ReadString('TLangForm', 'Caption', 'About');
        LangsList.BoundLabel.Caption := ReadString('TLangForm', 'LangsList.BoundLabel.Caption', 'About');
        LangsList.ItemsEx[0].Caption := ReadString('TLangForm', 'LangsList.ItemsEx.(0).Caption', 'About');
        LangsList.ItemsEx[1].Caption := ReadString('TLangForm', 'LangsList.ItemsEx.(1).Caption', 'About');
        LangsList.ItemsEx[2].Caption := ReadString('TLangForm', 'LangsList.ItemsEx.(2).Caption', 'About');
        LangsList.ItemsEx[3].Caption := ReadString('TLangForm', 'LangsList.ItemsEx.(3).Caption', 'About');
        LangsList.ItemsEx[4].Caption := ReadString('TLangForm', 'LangsList.ItemsEx.(4).Caption', 'About');
        LangsList.ItemsEx[5].Caption := ReadString('TLangForm', 'LangsList.ItemsEx.(5).Caption', 'About');
        LangsList.ItemsEx[6].Caption := ReadString('TLangForm', 'LangsList.ItemsEx.(6).Caption', 'About');
        LangsList.ItemsEx[7].Caption := ReadString('TLangForm', 'LangsList.ItemsEx.(7).Caption', 'About');
      end;
      // TLogForm
      with LogForm do
      begin
        Caption := ReadString('TLogForm', 'Caption', 'Logs');
        sPageControl1.Pages[0].Caption := ReadString('TLogForm', 'sPageControl1.sTabSheet1.Caption', 'Program log');
        sPageControl1.Pages[1].Caption := ReadString('TLogForm', 'sPageControl1.sTabSheet2.Caption', 'File add log');
        sPageControl1.Pages[2].Caption := ReadString('TLogForm', 'sPageControl1.sTabSheet3.Caption', 'Command line log');
        sPageControl1.Pages[3].Caption := ReadString('TLogForm', 'sPageControl1.sTabSheet4.Caption', 'Error log');
        SaveBtn.Caption := ReadString('TLogForm', 'sPanel1.SaveBtn.Caption', 'Save');
        ClearBtn.Caption := ReadString('TLogForm', 'sPanel1.ClearBtn.Caption', 'Clear');
        sBitBtn3.Caption := ReadString('TLogForm', 'sPanel1.sBitBtn3.Caption', 'Close');
        sSaveDialog1.Filter := ReadString('TLogForm', 'sSaveDialog1.Filter', 'Text Files|*.txt');
      end;
      // TMainForm
      with MainForm do
      begin
        Caption := ReadString('TMainForm', 'Caption', 'TEBookConverter');
        ProgressList.Columns[0].Caption := ReadString('TMainForm', 'ConvertPnl.ProgressList.Columns.(0).Caption', 'File Name');
        ProgressList.Columns[0].Caption := ReadString('TMainForm', 'ConvertPnl.ProgressList.Columns.(1).Caption', 'Size');
        OpenOutBtn2.Caption := ReadString('TMainForm', 'ConvertPnl.sPanel1.OpenOutBtn2.Caption', 'Open');
        StopBtn.Caption := ReadString('TMainForm', 'ConvertPnl.sPanel1.StopBtn.Caption', 'Stop');
        LogBtn2.Caption := ReadString('TMainForm', 'ConvertPnl.sPanel1.LogBtn2.Caption', 'Logs');
        sStatusBar1.Panels[0].Text := ReadString('TMainForm', 'sStatusBar1.Panels.(0).Text{1}', 'TEBookConverter');
        sStatusBar1.Panels[2].Text := ReadString('TMainForm', 'sStatusBar1.Panels.(2).Text', 'Uses Calibre');
        AddPnl.Caption := ReadString('TMainForm', 'AddPnl.Caption', 'Adding files, please wait...');
        AbortAddBtn.Caption := ReadString('TMainForm', 'AddPnl.AbortAddBtn.Caption', 'Abort');
        FileCountLabel.Caption := ReadString('TMainForm', 'NormalPnl.FileCountLabel.Caption', 'About');
        AddBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.AddBtn.Hint', 'About');
        AddBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.AddBtn.Caption', 'About');
        RemoveBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.RemoveBtn.Hint', 'About');
        RemoveBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.RemoveBtn.Caption', 'About');
        StartBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.StartBtn.Hint', 'About');
        StartBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.StartBtn.Caption', 'About');
        HelpBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.HelpBtn.Hint', 'About');
        HelpBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.HelpBtn.Caption', 'About');
        LogBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.LogBtn.Hint', 'About');
        LogBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.LogBtn.Caption', 'About');
        InfoBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.InfoBtn.Hint', 'About');
        InfoBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.InfoBtn.Caption', 'About');
        ReaderBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.ReaderBtn.Hint', 'About');
        ReaderBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.ReaderBtn.Caption', 'About');
        DonateBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.DonateBtn.Hint', 'About');
        DonateBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.DonateBtn.Caption', 'About');
        LangBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.LangBtn.Hint', 'About');
        LangBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.LangBtn.Caption', 'About');
        ClearBtn.Hint := ReadString('TMainForm', 'NormalPnl.TopBar.ClearBtn.Hint', 'About');
        ClearBtn.Caption := ReadString('TMainForm', 'NormalPnl.TopBar.ClearBtn.Caption', 'About');
        OutEdit.Hint := ReadString('TMainForm', 'NormalPnl.BottomBar.OutEdit.Hint', 'About');
        OutEdit.BoundLabel.Caption := ReadString('TMainForm', 'NormalPnl.BottomBar.OutEdit.BoundLabel.Caption', 'About');
        FormatList.Hint := ReadString('TMainForm', 'NormalPnl.BottomBar.FormatList.Hint', 'About');
        FormatList.BoundLabel.Caption := ReadString('TMainForm', 'NormalPnl.BottomBar.FormatList.BoundLabel.Caption', 'About');
        FormatList.Text := ReadString('TMainForm', 'NormalPnl.BottomBar.FormatList.Text', 'About');
        OpenOutBtn.Hint := ReadString('TMainForm', 'NormalPnl.BottomBar.OpenOutBtn.Hint', 'About');
        OpenOutBtn.Caption := ReadString('TMainForm', 'NormalPnl.BottomBar.OpenOutBtn.Caption', 'About');
        ProcessList.Hint := ReadString('TMainForm', 'NormalPnl.BottomBar.ProcessList.Hint', 'About');
        ProcessList.BoundLabel.Caption := ReadString('TMainForm', 'NormalPnl.BottomBar.ProcessList.BoundLabel.Caption', 'About');
        CreateFolderBtn.Hint := ReadString('TMainForm', 'NormalPnl.BottomBar.CreateFolderBtn.Hint', 'About');
        CreateFolderBtn.Caption := ReadString('TMainForm', 'NormalPnl.BottomBar.CreateFolderBtn.Caption', 'About');
        BrandsList.Hint := ReadString('TMainForm', 'NormalPnl.BottomBar.BrandsList.Hint', 'About');
        BrandsList.BoundLabel.Caption := ReadString('TMainForm', 'NormalPnl.BottomBar.BrandsList.BoundLabel.Caption', 'About');
        BrandsList.Text := ReadString('TMainForm', 'NormalPnl.BottomBar.BrandsList.Text', 'About');
        DeviceList.Hint := ReadString('TMainForm', 'NormalPnl.BottomBar.DeviceList.Hint', 'About');
        DeviceList.BoundLabel.Caption := ReadString('TMainForm', 'NormalPnl.BottomBar.DeviceList.BoundLabel.Caption', 'About');
        DontIfSameBtn.Hint := ReadString('TMainForm', 'NormalPnl.BottomBar.DontIfSameBtn.Hint', 'About');
        DontIfSameBtn.Caption := ReadString('TMainForm', 'NormalPnl.BottomBar.DontIfSameBtn.Caption', 'About');
        FileList.Hint := ReadString('TMainForm', 'NormalPnl.FileList.Hint', 'About');
        FileList.BoundLabel.Caption := ReadString('TMainForm', 'NormalPnl.FileList.BoundLabel.Caption', 'About');
        FileList.Columns[0].Caption := ReadString('TMainForm', 'NormalPnl.FileList.Columns.(0).Caption', 'About');
        FileList.Columns[1].Caption := ReadString('TMainForm', 'NormalPnl.FileList.Columns.(1).Caption', 'About');
        FileList.Columns[2].Caption := ReadString('TMainForm', 'NormalPnl.FileList.Columns.(2).Caption', 'About');
        OpenDialog.Filter := ReadString('TMainForm', 'OpenDialog.Filter', 'About');
        A1.Caption := ReadString('TMainForm', 'AddMenu.A1.Caption', 'About');
        A2.Caption := ReadString('TMainForm', 'AddMenu.A2.Caption', 'About');
        A3.Caption := ReadString('TMainForm', 'AddMenu.A3.Caption', 'About');
        A4.Caption := ReadString('TMainForm', 'HelpMenu.A4.Caption', 'About');
        C1.Caption := ReadString('TMainForm', 'HelpMenu.C1.Caption', 'About');
        H1.Caption := ReadString('TMainForm', 'HelpMenu.H1.Caption', 'About');
        D1.Caption := ReadString('TMainForm', 'HelpMenu.D1.Caption', 'About');
        S1.Caption := ReadString('TMainForm', 'HelpMenu.S1.Caption', 'About');
        T1.Caption := ReadString('TMainForm', 'HelpMenu.T1.Caption', 'About');
        C2.Caption := ReadString('TMainForm', 'HelpMenu.C2.Caption', 'About');
        D2.Caption := ReadString('TMainForm', 'DonateMenu.D2.Caption', 'About');
        D3.Caption := ReadString('TMainForm', 'DonateMenu.D3.Caption', 'About');
      end;
      // ResourceStrings
      with MainForm do
      begin
        UnitMain.RS_1 := ReadString('ResourceStrings', '64891_UnitMain_RS_1', '');
        UnitMain.RS_2 := ReadString('ResourceStrings', '64892_UnitMain_RS_2', '');
        UnitMain.RS_3 := ReadString('ResourceStrings', '64893_UnitMain_RS_3', '');
        UnitMain.RS_4 := ReadString('ResourceStrings', '64894_UnitMain_RS_4', '');
        UnitMain.RS_5 := ReadString('ResourceStrings', '64895_UnitMain_RS_5', '');
        UnitMain.RS_6 := ReadString('ResourceStrings', '64864_UnitMain_RS_6', '');
        UnitMain.RS_7 := ReadString('ResourceStrings', '64865_UnitMain_RS_7', '');
        UnitMain.RS_8 := ReadString('ResourceStrings', '64866_UnitMain_RS_8', '');
        UnitMain.RS_9 := ReadString('ResourceStrings', '64867_UnitMain_RS_9', '');
        UnitMain.RS_10 := ReadString('ResourceStrings', '64868_UnitMain_RS_10', '');
        UnitMain.RS_11 := ReadString('ResourceStrings', '64869_UnitMain_RS_11', '');
        UnitMain.RS_12 := ReadString('ResourceStrings', '64870_UnitMain_RS_12', '');
        UnitMain.RS_13 := ReadString('ResourceStrings', '64871_UnitMain_RS_13', '');
        UnitMain.RS_14 := ReadString('ResourceStrings', '64872_UnitMain_RS_14', '');
        UnitMain.RS_15 := ReadString('ResourceStrings', '64873_UnitMain_RS_15', '');
        UnitMain.RS_16 := ReadString('ResourceStrings', '64874_UnitMain_RS_16', '');
        UnitMain.RS_17 := ReadString('ResourceStrings', '64875_UnitMain_RS_17', '');
        UnitMain.RS_18 := ReadString('ResourceStrings', '64876_UnitMain_RS_18', '');
        UnitMain.RS_19 := ReadString('ResourceStrings', '64877_UnitMain_RS_19', '');
        UnitMain.RS_20 := ReadString('ResourceStrings', '64878_UnitMain_RS_20', '');
        UnitMain.RS_21 := ReadString('ResourceStrings', '64879_UnitMain_RS_21', '');
        UnitMain.RS_22 := ReadString('ResourceStrings', '64848_UnitMain_RS_22', '');
        UnitMain.RS_23 := ReadString('ResourceStrings', '64849_UnitMain_RS_23', '');
        UnitMain.RS_24 := ReadString('ResourceStrings', 'RS_24', '');
        UnitMain.RS_25 := ReadString('ResourceStrings', '64850_UnitMain_RS_25', '');
      end;
    end;
  finally
    LLangFile.Free;
  end;
end;

procedure TLangForm.SaveLoadLang(const Saving: Boolean);
var
  LSetFile: TIniFile;
begin
  LSetFile := TIniFile.Create(MainForm.FAppData + '\settings.ini');
  try
    if Saving then
    begin
      LSetFile.WriteInteger('lang', 'index', LangsList.ItemIndex);
    end
    else
    begin
      LangsList.ItemIndex := LSetFile.ReadInteger('lang', 'index', 1);
      LangsListChange(self);
    end;
  finally
    LSetFile.Free;
    // MainForm.LoadSettings;
  end;
end;

end.
