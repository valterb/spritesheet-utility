{
  Copyright 2019 Valter Buccinà.

  "SpriteSheet Utility" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.
  https://github.com/valterb/spritesheet-utility/blob/master/LICENSE

  See the README.md file for details on using the software.
  https://github.com/valterb/spritesheet-utility/blob/master/README.md

  "SpriteSheet Utility" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}


unit UnitMain;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ShellCtrls, ComCtrls, ExtCtrls, Menus, Buttons, ShellApi,
  CastleScene, CastleControls, CastleFilesUtils, CastleKeysMouse, CastleLog,
  CastleUtils, CastleUIControls, CastleGLImages, CastleGLUtils, CastleTimeUtils,
  CastleControl, CastleDialogs, CastleControlsImages, CastleImages;

const
  ResizeInterpol: Array [0..13] of TResizeInterpolation = (riNearest,
                                                           riBilinear,
                                                           riMitchel,
                                                           riBlackman,
                                                           riBlackmanSinc,
                                                           riBlackmanBessel,
                                                           riGaussian,
                                                           riHermite,
                                                           riLanczos,
                                                           riQuadratic,
                                                           riCubic,
                                                           riCatrom,
                                                           riHanning,
                                                           riHamming);

type

  { TMainForm }

  TMainForm = class(TForm)
    BitBtn1: TBitBtn;
    BtnFlipBoth: TBitBtn;
    BtnFlipHorz: TBitBtn;
    BtnFlipVert: TBitBtn;
    BtnPreview: TButton;
    BtnSaveFlip: TBitBtn;
    BtnSelectImage: TBitBtn;
    BtnExecute: TBitBtn;
    BtnSelectSpriteSheet: TBitBtn;
    CastleControl1: TCastleControl;
    CBResizeKeepProp: TCheckBox;
    CMBResizeInterpolationMode: TComboBox;
    EdResizeH: TEdit;
    EdResizeW: TEdit;
    GroupBox1: TGroupBox;
    ImgListView: TShellListView;
    ItemSel: TLabel;
    Label1: TLabel;
    LblItemsNum: TLabel;
    LblResizeH: TLabel;
    LblResizeInterpolationMode: TLabel;
    LblResizeW: TLabel;
    SelectFirstImg: TCastleOpenImageDialog;
    SelectSpriteSheet: TCastleOpenImageDialog;
    CBCreateSpriteSheet: TCheckBox;
    CBPreviewSpriteSheet: TCheckBox;
    EdSpriteSheetName: TEdit;
    EdNumCols: TEdit;
    EdPadding: TEdit;
    EdPreviewFrameCount: TEdit;
    EdPreviewFramePerSec: TEdit;
    EdPreviewHorzSizeFrame: TEdit;
    EdPreviewNumCols: TEdit;
    EdPreviewVertSizeFrame: TEdit;
    GBCreateSpriteSheet: TGroupBox;
    GBPreviewSpriteSheet: TGroupBox;
    LblColumns: TLabel;
    LblPreviewVertSizeFrame: TLabel;
    LblSpriteSheetName: TLabel;
    LblPreviewFramePerSec: TLabel;
    LblPreviewFrameCount: TLabel;
    LblPreviewNumCols: TLabel;
    LclPreviewHorzSizeFrame: TLabel;
    LblPadding: TLabel;
    PnlTop: TPanel;
    PnlRight: TPanel;
    PnlImages: TPanel;
    PnlLeft: TPanel;
    PnlFlip: TPanel;
    ScrollBoxImgs: TScrollBox;
    ScrollBoxMainImage: TScrollBox;
    ImgsSquare: TShape;
    UDNumCols: TUpDown;
    UDPadding: TUpDown;
    UDPreviewFrameCount: TUpDown;
    UDPreviewFramePerSec: TUpDown;
    UDPreviewHorzSizeFrame: TUpDown;
    UDPreviewNumCols: TUpDown;
    UDPreviewVertSizeFrame: TUpDown;
    UDResizeW: TUpDown;
    UDResizeH: TUpDown;
    procedure BitBtn1Click(Sender: TObject);
    procedure BtnFlipBothClick(Sender: TObject);
    procedure BtnFlipVertClick(Sender: TObject);
    procedure BtnFlipHorzClick(Sender: TObject);
    procedure BtnSaveFlipClick(Sender: TObject);
    procedure BtnExecuteClick(Sender: TObject);
    procedure BtnSelectImageClick(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
    procedure BtnSelectSpriteSheetClick(Sender: TObject);
    procedure CastleControl1Render(Sender: TObject);
    procedure CastleControl1Update(Sender: TObject);
    procedure CBCreateSpriteSheetChange(Sender: TObject);
    procedure CBPreviewSpriteSheetChange(Sender: TObject);
    procedure CBResizeKeepPropChange(Sender: TObject);
    procedure EdPreviewFrameCountChange(Sender: TObject);
    procedure EdPreviewFramePerSecChange(Sender: TObject);
    procedure EdPreviewNumColsChange(Sender: TObject);
    procedure EdResizeHChange(Sender: TObject);
    procedure EdResizeWChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImgListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

  private
    CreateScrollBar: TLabel;
    ImgExt, ImgName: String;
    Imgs: Array of TImage;
    ListFileName: TStringList;
    MainCastleImage: TCastleImage;
    MainImage: TGLImage;
    ResizeCastleImage: TCastleImage;
    procedure ActivateScrollBar(HorzSize, VertSize: Integer);
    procedure ChangingTypeOfSpriteSheet;
    procedure DisableCreateSpriteSheet;
    procedure DisableEditImage;
    procedure DisableSpriteSheetPreview;
    procedure EnableCreateSpriteSheet;
    procedure EnableEditImage;
    procedure EnableSpriteSheetPreview;
    procedure LoadImages(FirstImage: String);
    procedure NilAndFree;
    procedure ResetCreateSpriteSheetData;
    procedure ResetPreviewSpriteSheetData;
    procedure ResetShowImage;
    procedure SetSpriteSheetPreview;
    procedure ShowMainImage(Item: String; Index: Integer);

  public

  end;

var
  MainForm: TMainForm;
  Player: TSprite;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ShowMainImage(Item: String; Index: Integer);
var
  LoadPath: string;
begin
  LoadPath := IncludeTrailingPathDelimiter(ImgListView.Root) + Item;
  if not FileExists(LoadPath) then
    exit;
  begin
    if Assigned(MainImage) then
      FreeAndNil(MainImage);
    if Assigned(MainCastleImage) then
      FreeAndNil(MainCastleImage);
    if Assigned(ResizeCastleImage) then
      FreeAndNil(ResizeCastleImage);
    ImgsSquare.Left := 70 * Index;
    ScrollBoxImgs.HorzScrollBar.Position := ImgsSquare.Left;
    ImgsSquare.Visible := True;
    try
      MainCastleImage := LoadImage(LoadPath);
      ResizeCastleImage := MainCastleImage.MakeCopy;
      MainImage := TGLImage.Create(MainCastleImage, True {smooth scaling}, False {doesn't own CastleImage});
      ActivateScrollBar(0, 0);
      if (CBCreateSpriteSheet.Checked) then
      begin
        EnableEditImage;
        EdResizeW.Text := IntToStr(MainImage.Width);
        EdResizeH.Text := IntToStr(MainImage.Height);
      end
        else if (CBPreviewSpriteSheet.Checked) then
        begin
          EnableSpriteSheetPreview;
          EdPreviewHorzSizeFrame.Text := IntToStr(MainImage.Width);
          EdPreviewVertSizeFrame.Text := IntToStr(MainImage.Height);
        end;
    except
      on E: Exception do
        ShowMessage('Error: ' + E.ClassName + #13#10 + E.Message);
    end;
  end;
end;

procedure TMainForm.CastleControl1Render(Sender: TObject);
var
  ImageX, ImageY: Integer;
begin
  if Assigned(Player) then
  begin
    ImageX := (CastleControl1.Width div 2) - (Player.FrameWidth div 2);
    ImageY := CastleControl1.Height div 2 - Player.FrameHeight div 2;
    Player.Draw(ImageX, ImageY);
  end
    else if (Assigned(MainImage) and (CBCreateSpriteSheet.Checked)) then
    begin
      ImageX := (CastleControl1.Width div 2) - (MainImage.Width div 2);
      ImageY := CastleControl1.Height div 2 - MainImage.Height div 2;
      MainImage.Draw(ImageX, ImageY);
    end;
end;

procedure TMainForm.SetSpriteSheetPreview;
var
  HorzSizeFrame, VertSizeFrame, VirtualFrame, FrameCount, ColCount: Integer;
begin
  if Assigned(MainImage) then
  begin
    FrameCount := StrToInt(EdPreviewFrameCount.Text);
    ColCount := StrToInt(EdPreviewNumCols.Text);
    HorzSizeFrame := MainImage.Width div StrToInt(EdPreviewNumCols.Text);
    while (FrameCount mod ColCount <> 0) do
      Inc(FrameCount);
    VirtualFrame := FrameCount div ColCount;
    VertSizeFrame := MainImage.Height div VirtualFrame;
    EdPreviewHorzSizeFrame.Text := IntToStr(HorzSizeFrame);
    EdPreviewVertSizeFrame.Text := IntToStr(VertSizeFrame);
  end
end;

procedure TMainForm.BtnPreviewClick(Sender: TObject);
var
  Path: String;
  FrameCount, Columns, HorzSizeFrame, VertSizeFrame, FramePerSec: Integer;
begin
  if BtnPreview.Caption = 'Start' then
  begin
    HorzSizeFrame := StrToInt(EdPreviewHorzSizeFrame.Text);
    VertSizeFrame := StrToInt(EdPreviewVertSizeFrame.Text);
    if ((HorzSizeFrame = 0) or (VertSizeFrame = 0)) then
    begin
      ShowMessage('Set horz and vert size frame properly' + #10#13 +
                  'or change the value of Frame count and Columns' + #10#13 +
                  'in order to obtain a possible result');
      Exit;
    end;
    FrameCount := StrToInt(EdPreviewFrameCount.Text);
    Columns := StrToInt(EdPreviewNumCols.Text);
    FramePerSec := StrToInt(EdPreviewFramePerSec.Text);
    BtnPreview.Caption := 'Stop';
    FreeAndNil(Player);
    ActivateScrollBar(HorzSizeFrame, VertSizeFrame);
    try
      Path := IncludeTrailingPathDelimiter(ImgListView.Root) + ImgListView.Selected.Caption; // ImgListView.Items.Item[imglistview.Selected.Index].Caption;
      Player := TSprite.CreateFrameSize(Path,
                                        FrameCount,                   // frames count
                                        Columns,                      // columns (number of frames in a row in the image)
                                        HorzSizeFrame, VertSizeFrame, // size of a single frame
                                        False,                        // smooth scaling
                                        True                          // time loop
                                       );
      Player.FramesPerSecond := FramePerSec;
      Player.Play;
    except
      on E: Exception do
      begin
        ShowMessage('Error: ' + E.ClassName + #13#10 + E.Message);
        BtnPreview.Caption := 'Start';
      end;
    end;
  end
    else
    begin
      BtnPreview.Caption := 'Start';
      FreeAndNil(Player);
    end;
end;

procedure TMainForm.CastleControl1Update(Sender: TObject);
var
  SecondsPassed: TFloatTime;
begin
  if Assigned(Player) then
  begin
    SecondsPassed := CastleControl1.Fps.SecondsPassed;
    Player.Update(SecondsPassed);
  end;
end;

procedure TMainForm.CBCreateSpriteSheetChange(Sender: TObject);
begin
  if CBCreateSpriteSheet.Checked then
  begin
    CBPreviewSpriteSheet.Checked := false;
    if BtnPreview.Caption = 'Stop' then
       BtnPreviewClick(Self);
    ResetPreviewSpriteSheetData;
    SelectSpriteSheet.FileName := '';
    ChangingTypeOfSpriteSheet;
    BtnSelectImage.Enabled := True;
    DisableSpriteSheetPreview;
  end
    else
      CBPreviewSpriteSheet.Checked := true;
end;

procedure TMainForm.CBPreviewSpriteSheetChange(Sender: TObject);
begin

  if CBPreviewSpriteSheet.Checked then
  begin
    CBCreateSpriteSheet.Checked := false;
    ResetCreateSpriteSheetData;
    SelectFirstImg.FileName := '';
    ChangingTypeOfSpriteSheet;
    BtnSelectSpriteSheet.Enabled := True;
    DisableCreateSpriteSheet;
    DisableEditImage;
  end
    else
      CBCreateSpriteSheet.Checked := true;
end;

procedure TMainForm.EdPreviewFrameCountChange(Sender: TObject);
begin
  if ((EdPreviewFrameCount.Text = '') or (EdPreviewFrameCount.Text = '0')) then
  begin
    EdPreviewFrameCount.Text := '1';
    EdPreviewFrameCount.SetFocus;
    EdPreviewFrameCount.SelectAll;
    Exit;
  end;
  EdPreviewFramePerSec.Text := EdPreviewFrameCount.Text;
  SetSpriteSheetPreview;
end;

procedure TMainForm.EdPreviewFramePerSecChange(Sender: TObject);
begin
  if Assigned(Player) then
    if Player.Playing then
      Player.FramesPerSecond := StrToInt(EdPreviewFramePerSec.Text);
end;

procedure TMainForm.EdPreviewNumColsChange(Sender: TObject);
begin
  if ((EdPreviewNumCols.Text = '') or (EdPreviewNumCols.Text = '0')) then
  begin
    EdPreviewNumCols.Text := '1';
    EdPreviewNumCols.SetFocus;
    EdPreviewNumCols.SelectAll;
    Exit;
  end;
  SetSpriteSheetPreview;
end;

procedure TMainForm.EdResizeHChange(Sender: TObject);
var
  W, H: Cardinal;
  Mode: TResizeInterpolation;
begin
  if ((EdResizeH.Text = '') or (EdResizeH.Text = '0')) then
  begin
    EdResizeH.Text := '0';
    EdResizeH.SetFocus;
    EdResizeH.SelectAll;
    Exit;
  end;
  if Assigned(MainImage) then
  begin
    H := StrToInt(EdResizeH.Text);
    W := StrToInt(EdResizeW.Text);
    if H <> MainCastleImage.Height then
    begin
      if (CBResizeKeepProp.Checked) then
        W := (H * ResizeCastleImage.Width) div ResizeCastleImage.Height;
      Mode := ResizeInterpol[CMBResizeInterpolationMode.ItemIndex];
      MainCastleImage.Assign(ResizeCastleImage);
      MainCastleImage.Resize(W, H, Mode,'');
      MainImage.Load(MainCastleImage);
      EdResizeW.Text := IntToStr(W);
      ActivateScrollBar(MainImage.Width, MainImage.Height);
    end;
  end;
end;

procedure TMainForm.EdResizeWChange(Sender: TObject);
var
  W, H: Cardinal;
  Mode: TResizeInterpolation;
begin
  if ((EdResizeW.Text = '') or (EdResizeW.Text = '0')) then
  begin
    EdResizeW.Text := '0';
    EdResizeW.SetFocus;
    EdResizeW.SelectAll;
    Exit;
  end;
  if Assigned(MainImage) then
  begin
    W := StrToInt(EdResizeW.Text);
    H := StrToInt(EdResizeH.Text);
    if  W <> MainCastleImage.Width then
    begin
      if (CBResizeKeepProp.Checked) then
        H := (W * ResizeCastleImage.Height) div ResizeCastleImage.Width;
      Mode := ResizeInterpol[CMBResizeInterpolationMode.ItemIndex];
      MainCastleImage.Assign(ResizeCastleImage);
      MainCastleImage.Resize(W, H, Mode, '');
      MainImage.Load(MainCastleImage);
      EdResizeH.Text := IntToStr(H);
      ActivateScrollBar(MainImage.Width, MainImage.Height);
    end;
  end;
end;

procedure TMainForm.CBResizeKeepPropChange(Sender: TObject);
var
  Mode: TResizeInterpolation;
begin
  if Assigned(MainImage) then
  begin
    if CBResizeKeepProp.Checked then
    begin
      Mode := ResizeInterpol[CMBResizeInterpolationMode.ItemIndex];
      ResizeCastleImage.Resize(MainImage.Width, MainImage.Height, Mode, '');
    end;
  end;
end;

procedure TMainForm.ResetShowImage;
var
  i: Integer;
begin
  if Assigned(Imgs) then
  begin
    for i := Low(imgs) to High(imgs) do
      FreeAndNil(imgs[i]);
    SetLength(imgs, 0);
  end;
  NilAndFree;
end;

procedure TMainForm.LoadImages(FirstImage: String);
var
  Info : TSearchRec;
  Count, i, x, y: integer;
  ShowImage: string;
begin
  ResetShowImage;
  ListFileName := TStringList.Create;
  ImgListView.Root := ExtractFileDir(FirstImage);
  i := LastDelimiter('.' + PathDelim + DriveDelim, FirstImage);
  if ((i =0 )  or  (FirstImage[i] <> '.')) then
    i := MaxInt;
  ShowImage := ExtractFileName(Copy(FirstImage, 1, i-1));
  i := LastDelimiter('_', ShowImage);
  ImgName := copy(ShowImage, 1, i-1);
  ImgExt := ExtractFileExt(FirstImage);
  EdSpriteSheetName.Text := 'SpriteSheet' + ImgName;
  Count := 0;
  if (SysUtils.FindFirst(IncludeTrailingPathDelimiter(ImgListView.Root) + '*', faAnyFile and (not faDirectory), Info) = 0) then
  begin
    repeat
      with Info do
      begin
        if (not(Info.Name = '.') and not(Info.Name = '..')) then
        begin
          ListFileName.Add(Info.Name);
          Inc(Count);
        end;
      end;
    until (SysUtils.FindNext(Info) <> 0);
    SysUtils.FindClose(Info);
  end;
  LblItemsNum.Caption := 'Items: ' + IntToStr(Count);
  SetLength(imgs, Count);
  y := 5;
  x := 5;
  for i := 0 to Count-1 do
  begin
    imgs[i] := TImage.Create(Self);
    with imgs[i] do
    begin
      Parent := ScrollBoxImgs;
      width := 70;
      Height := 70;
      Picture.LoadFromFile(IncludeTrailingPathDelimiter(ImgListView.Root) + ListFileName[i]);
      Proportional := True;
      Stretch := True;
      Center := True;
      if (i > 0) then
      begin
        x := x + 70;
        Left := x;
      end;
      Left := x;
      Top := y;
    end;
  end;
end;

procedure TMainForm.BtnExecuteClick(Sender: TObject);
var
  SavePath, SaveDirSpriteSheet, SaveFileSpriteSheet, SaveSpriteSheetPath, CmdString: string;
  i: Integer;
  SavedFile: Boolean;
begin
  if (EdNumCols.Text = '0') then
  begin
    ShowMessage('Columns can not be zero');
    Exit;
  end;
  if (EdSpriteSheetName.Text = '') then
  begin
    ShowMessage('Give a name to the spritesheet');
    Exit;
  end;
  SavePath := IncludeTrailingPathDelimiter(ImgListView.Root);
  SaveDirSpriteSheet := SavePath + 'SpriteSheet';
  SaveFileSpriteSheet := SavePath + IncludeTrailingPathDelimiter('SpriteSheet');
  SaveSpriteSheetPath := SaveFileSpriteSheet + EdSpriteSheetName.Text + '.png';
  SavedFile := False;
  try
    if not DirectoryExists(SaveDirSpriteSheet) then
      CreateDir(SaveDirSpriteSheet);
    CmdString := SavePath + ImgName + '_@counter(' + EdPadding.Text + ')' + ImgExt + ' ' +
                 SaveSpriteSheetPath +
                 ' ' +
                 EdNumCols.Text;
    ShellExecute(0, nil, 'combine_images_into_sprite_sheet.exe', PChar(CmdString), nil, SW_HIDE);
  except
    on E: Exception do
      ShowMessage('Error: ' + E.ClassName + #13#10 + E.Message);
  end;
  for I := 1 to 20 do
  begin
    Sleep(100);
    if FileExists(SaveSpriteSheetPath) then
    begin
      ShowMessage('SpriteSheet saved in SpriteSheet folder');
      SavedFile := True;
      Break;
    end;
  end;
  if (not SavedFile) then
    ShowMessage('Something went wrong');
end;

procedure TMainForm.BtnSaveFlipClick(Sender: TObject);
var
  SavePath, SaveImgExt, SaveImgName, SaveImgPath: string;
  i: Integer;
  SavedFile: Boolean;
begin
  if Assigned(MainImage) then
  begin
    SavePath := IncludeTrailingPathDelimiter(ImgListView.Root);
    try
      if not DirectoryExists(SavePath + 'EditedImages') then
        CreateDir(SavePath + 'EditedImages');
      i := LastDelimiter('.' + PathDelim + DriveDelim, ImgListView.Selected.Caption);
      if ((i=0)  or  (ImgListView.Selected.Caption[i] <> '.')) then
        i := MaxInt;
      SaveImgName := ExtractFileName(Copy(ImgListView.Selected.Caption, 1, i-1));
      SaveImgExt := ExtractFileExt(ImgListView.Selected.Caption);
      SaveImgPath := SavePath + IncludeTrailingPathDelimiter('EditedImages') + '(Edited)_' + SaveImgName + SaveImgExt;
      SaveImage(MainCastleImage, SaveImgPath);
    except
      on E: Exception do
        ShowMessage('Error: ' + E.ClassName + #13#10 + E.Message);
    end;
    for i := 1 to 20 do
    begin
      Sleep(100);
      if FileExists(SaveImgPath) then
      begin
        ShowMessage('Image saved in EditedImages folder');
        SavedFile := True;
        Break;
      end;
    end;
    if (not SavedFile) then
      ShowMessage('Something went wrong');
  end;
end;

procedure TMainForm.BtnFlipHorzClick(Sender: TObject);
begin
  if Assigned(MainImage) then
  begin
    MainCastleImage.FlipHorizontal;
    MainImage.Load(MainCastleImage);
  end;
end;

procedure TMainForm.BtnFlipVertClick(Sender: TObject);
begin
  if Assigned(MainImage) then
  begin
    MainCastleImage.FlipVertical;
    MainImage.Load(MainCastleImage);
  end;
end;

procedure TMainForm.BtnFlipBothClick(Sender: TObject);
begin
  if Assigned(MainImage) then
  begin
    MainCastleImage.FlipHorizontal;
    MainCastleImage.FlipVertical;
    MainImage.Load(MainCastleImage);
  end;
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.BtnSelectImageClick(Sender: TObject);
begin
  if SelectFirstImg.Execute then
  begin
    ResetCreateSpriteSheetData;
    EnableCreateSpriteSheet;
    DisableEditImage;
    LoadImages(SelectFirstImg.FileName);
  end;
end;


procedure TMainForm.BtnSelectSpriteSheetClick(Sender: TObject);
begin
  if SelectSpriteSheet.Execute then
  begin
    ResetPreviewSpriteSheetData;
    LoadImages(SelectSpriteSheet.FileName);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  NilAndFree;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ResetShowImage;
end;

procedure TMainForm.ImgListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Assigned(Player) then
    if (Player.Playing) then
      exit;
  begin
    ItemSel.Caption := Item.Caption;
    ShowMainImage(Item.Caption, ImgListView.ItemIndex);
  end;
end;

procedure TMainForm.EnableCreateSpriteSheet;
begin
  EdPadding.Enabled := True;
  EdNumCols.Enabled := True;
  EdSpriteSheetName.Enabled := True;
  BtnExecute.Enabled := True;
end;

procedure TMainForm.DisableCreateSpriteSheet;
begin
  BtnSelectImage.Enabled := False;
  EdPadding.Enabled := False;
  EdNumCols.Enabled := False;
  EdSpriteSheetName.Enabled := False;
  BtnExecute.Enabled := False;
end;

procedure TMainForm.EnableEditImage;
begin
  BtnFlipHorz.Enabled := True;
  BtnFlipVert.Enabled := True;
  BtnFlipBoth.Enabled := True;
  EdResizeW.Enabled := True;
  EdResizeH.Enabled := True;
  CBResizeKeepProp.Enabled := True;
  CMBResizeInterpolationMode.Enabled := True;
  BtnSaveFlip.Enabled := True;
end;

procedure TMainForm.DisableEditImage;
begin
  BtnFlipHorz.Enabled := False;
  BtnFlipVert.Enabled := False;
  BtnFlipBoth.Enabled := False;
  EdResizeW.Enabled := False;
  EdResizeH.Enabled := False;
  CBResizeKeepProp.Enabled := False;
  CMBResizeInterpolationMode.Enabled := False;
  BtnSaveFlip.Enabled := False;
end;

procedure TMainForm.EnableSpriteSheetPreview;
begin
  EdPreviewFrameCount.Enabled := True;
  EdPreviewNumCols.Enabled := True;
  EdPreviewHorzSizeFrame.Enabled := True;
  EdPreviewVertSizeFrame.Enabled := True;
  EdPreviewFramePerSec.Enabled := True;
  BtnPreview.Enabled := True;
end;

procedure TMainForm.DisableSpriteSheetPreview;
begin
  BtnSelectSpriteSheet.Enabled := False;
  EdPreviewFrameCount.Enabled := False;
  EdPreviewNumCols.Enabled := False;
  EdPreviewHorzSizeFrame.Enabled := False;
  EdPreviewVertSizeFrame.Enabled := False;
  EdPreviewFramePerSec.Enabled := False;
  BtnPreview.Enabled := False;
end;

procedure TMainForm.ChangingTypeOfSpriteSheet;
begin
  ResetShowImage;
  ImgListView.Root := '';
  imglistview.Clear;
  ItemSel.Caption := '(none)';
  LblItemsNum.Caption := 'Items: 0';
  ImgsSquare.Visible := False;
end;

procedure TMainForm.ResetCreateSpriteSheetData;
begin
  UDPadding.Position := 0;
  EdPadding.Text := '0';
  UDNumCols.Position := 0;
  EdNumCols.Text := '0';
  EdSpriteSheetName.Clear;
  UDResizeW.Position := 0;
  EdResizeW.Text := '0';
  UDResizeH.Position := 0;
  EdResizeH.Text := '0';
  CBResizeKeepProp.Checked := False;
  CMBResizeInterpolationMode.ItemIndex := 1;
end;

procedure TMainForm.ResetPreviewSpriteSheetData;
begin
  UDPreviewFrameCount.Position := 1;
  EdPreviewFrameCount.Text := '1';
  UDPreviewNumCols.Position := 1;
  EdPreviewNumCols.Text := '1';
  UDPreviewHorzSizeFrame.Position := 0;
  EdPreviewHorzSizeFrame.Text := '0';
  UDPreviewVertSizeFrame.Position := 0;
  EdPreviewVertSizeFrame.Text := '0';
  UDPreviewFramePerSec.Position := 0;
  EdPreviewFramePerSec.Text := '0';
end;

procedure TMainForm.ActivateScrollBar(HorzSize, VertSize: Integer);
const
  OffSet: Integer = 10;
begin
  if Assigned(CreateScrollBar) then
    FreeAndNil(CreateScrollBar);
  CreateScrollBar := TLabel.Create(Self);
  CreateScrollBar.Parent := ScrollBoxMainImage;
  if (CBCreateSpriteSheet.Checked) then
  begin
    CreateScrollBar.Left := MainImage.Width + OffSet;
    CreateScrollBar.Top := MainImage.Height + OffSet;
  end
  else
    begin
      CreateScrollBar.Left := HorzSize + OffSet;
      CreateScrollBar.Top := VertSize + OffSet;
    end;
end;

procedure TMainForm.NilAndFree;
begin
  FreeAndNil(Player);
  FreeAndNil(MainCastleImage);
  FreeAndNil(ResizeCastleImage);
  FreeAndNil(MainImage);
  FreeAndNil(ListFileName);
  FreeAndNil(Imgs);
  FreeAndNil(CreateScrollBar);
end;

end.


