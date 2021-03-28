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

unit UnitLogs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, sSkinProvider, Vcl.StdCtrls, sMemo,
  Vcl.ComCtrls, sPageControl, Vcl.ExtCtrls, sPanel, Vcl.Buttons, sBitBtn,
  sDialogs;

type
  TLogForm = class(TForm)
    sPageControl1: TPageControl;
    sPanel1: TsPanel;
    sTabSheet1: TsTabSheet;
    sTabSheet2: TsTabSheet;
    sTabSheet3: TsTabSheet;
    sTabSheet4: TsTabSheet;
    MainList: TsMemo;
    FileAddList: TsMemo;
    CMDList: TsMemo;
    CalibreList: TsMemo;
    SaveBtn: TsBitBtn;
    ClearBtn: TsBitBtn;
    sBitBtn3: TsBitBtn;
    sSaveDialog1: TsSaveDialog;
    procedure sBitBtn3Click(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LogForm: TLogForm;

implementation

{$R *.dfm}

procedure TLogForm.ClearBtnClick(Sender: TObject);
begin
  case sPageControl1.ActivePageIndex of
    0:
      MainList.Lines.Clear;
    1:
      FileAddList.Lines.Clear;
    2:
      CMDList.Lines.Clear;
    3:
      CalibreList.Lines.Clear;
  end;
end;

procedure TLogForm.SaveBtnClick(Sender: TObject);
begin
  case sPageControl1.ActivePageIndex of
    0:
      sSaveDialog1.FileName := 'TEBookConverterLog.txt';
    1:
      sSaveDialog1.FileName := 'TEBookConverterFileAdd.txt';
    2:
      sSaveDialog1.FileName := 'TEBookConverterCMDLog.txt';
    3:
      sSaveDialog1.FileName := 'TEBookConverterErrorLog.txt';
  end;
  if sSaveDialog1.Execute then
  begin
    case sPageControl1.ActivePageIndex of
      0:
        MainList.Lines.SaveToFile(sSaveDialog1.FileName, TEncoding.UTF8);
      1:
        FileAddList.Lines.SaveToFile(sSaveDialog1.FileName, TEncoding.UTF8);
      2:
        CMDList.Lines.SaveToFile(sSaveDialog1.FileName, TEncoding.UTF8);
      3:
        CalibreList.Lines.SaveToFile(sSaveDialog1.FileName, TEncoding.UTF8);
    end;
  end;
end;

procedure TLogForm.sBitBtn3Click(Sender: TObject);
begin
  Self.Close;
end;

end.
