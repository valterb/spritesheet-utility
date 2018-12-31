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
    LblNumItems: TLabel;
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
    procedure CMBResizeInterpolationModeChange(Sender: TObject);
    procedure EdPreviewFrameCountChange(Sender: TObject);
    procedure EdPreviewFramePerSecChange(Sender: TObject);
    procedure EdPreviewNumColsChange(Sender: TObject);
    procedure EdResizeHChange(Sender: TObject);
    procedure EdResizeWChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImgListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

  private
    Imgs: Array of TImage;
    ImgExt, ImgName: String;
    MainImage: TGLImage;
    MainCastleImage: TCastleImage;
    ResizeCastleImage: TCastleImage;
    ListFileName: TStringList;
    CreateScrollBar: TLabel;
    procedure LoadImages(FirstImage: String);
    procedure ShowMainImage(Item: String; Index: Integer);
    procedure ResetShowImage;
    procedure SetSpriteSheetPreview;
    procedure EnableSpriteSheetPreview;
    procedure DisableSpriteSheetPreview;
    procedure EnableCreateSpriteSheet;
    procedure DisableCreateSpriteSheet;
    procedure ChangingTypeOfSpriteSheet;
    procedure ActivateScrollBar(HorzSize, VertSize: Integer);
    procedure NilAndFree;
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
      MainImage := TGLImage.Create(MainCastleImage, true {smooth scaling}, false {doesn't own CastleImage});
      ActivateScrollBar(0, 0);
      EdResizeW.Text := IntToStr(MainImage.Width);
      EdResizeH.Text := IntToStr(MainImage.Height);
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
    ImageX := (ScrollBoxMainImage.Width div 2) - (Player.FrameWidth div 2);
    ImageY := CastleControl1.Height div 2 - Player.FrameHeight div 2;
    Player.Draw(ImageX, ImageY);
  end
    else if (Assigned(MainImage) and (CBCreateSpriteSheet.Checked)) then
    begin
      ImageX := (ScrollBoxMainImage.Width div 2) - (MainImage.Width div 2);
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
    if (ImgListView.ItemIndex = -1) then
    begin
      ShowMessage('Select a spritesheet');
      Exit;
    end;
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
      Path := IncludeTrailingPathDelimiter(ImgListView.Root) + ImgListView.Items.Item[imglistview.Selected.Index].Caption;
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

procedure TMainForm.BtnSelectSpriteSheetClick(Sender: TObject);
begin
  if SelectSpriteSheet.Execute then
  begin
    EnableSpriteSheetPreview;
    LoadImages(SelectSpriteSheet.FileName);
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
    if BtnPreview.Caption = 'Stop' then
       BtnPreviewClick(Self);
    if (SelectSpriteSheet.FileName <> '') then
    begin
      SelectSpriteSheet.FileName := '';
      ChangingTypeOfSpriteSheet;
    end;
    CBPreviewSpriteSheet.Checked := False;
    BtnSelectImage.Enabled := True;
    DisableSpriteSheetPreview;
    if (SelectFirstImg.FileName <> '') then
      EnableCreateSpriteSheet;
  end
    else
    begin
      DisableCreateSpriteSheet;
    end;
end;

procedure TMainForm.CBPreviewSpriteSheetChange(Sender: TObject);
begin
  if CBPreviewSpriteSheet.Checked then
  begin
    if (SelectFirstImg.FileName <> '') then
    begin
      SelectFirstImg.FileName := '';
      ChangingTypeOfSpriteSheet;
    end;
    CBCreateSpriteSheet.Checked := False;
    BtnSelectSpriteSheet.Enabled := True;
    DisableCreateSpriteSheet;
    if (SelectSpriteSheet.FileName <> '') then
      EnableSpriteSheetPreview;
  end
    else
    begin
      DisableSpriteSheetPreview;
    end;
end;

procedure TMainForm.EdPreviewFrameCountChange(Sender: TObject);
begin
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
  if ((EdResizeW.Text = '') or (EdResizeH.Text = '0')) then
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
begin
  if CBResizeKeepProp.Checked then
  begin
    MainCastleImage.Assign(ResizeCastleImage);
    MainImage.Load(MainCastleImage);
    EdResizeW.Text := IntToStr(MainImage.Width);
    EdResizeH.Text := IntToStr(MainImage.Height);
  end;
end;

procedure TMainForm.CMBResizeInterpolationModeChange(Sender: TObject);
begin
  MainCastleImage.Assign(ResizeCastleImage);
  MainImage.Load(MainCastleImage);
  EdResizeW.Text := IntToStr(MainImage.Width);
  EdResizeH.Text := IntToStr(MainImage.Height);
  ActivateScrollBar(MainImage.Width, MainImage.Height);
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
  LblItemsNum.Caption:=IntToStr(Count);
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
  SavePath, SaveDirSpriteSheet, SaveFileSpriteSheet, CmdString: string;
begin
  if (EdNumCols.Text = '0') then
  begin
    ShowMessage('Columns can not be zero');
    Exit;
  end;
  SavePath := IncludeTrailingPathDelimiter(ImgListView.Root);
  SaveDirSpriteSheet := SavePath + 'SpriteSheet';
  SaveFileSpriteSheet := SavePath+IncludeTrailingPathDelimiter('SpriteSheet');
  try
    if not DirectoryExists(SaveDirSpriteSheet) then
      CreateDir(SaveDirSpriteSheet);
    CmdString := SavePath + ImgName + '_@counter(' + EdPadding.Text + ')' + ImgExt + ' ' +
                 SaveFileSpriteSheet + EdSpriteSheetName.Text + '.png ' +
                 EdNumCols.Text;
    ShellExecute(0, nil, 'combine_images_into_sprite_sheet.exe', PChar(CmdString), nil, SW_SHOWNORMAL);
  except
    on E: Exception do
      ShowMessage('Error: ' + E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TMainForm.BtnSaveFlipClick(Sender: TObject);
var
  SavePath, SaveImgExt, SaveImgName: string;
  i: Integer;
begin
  if Assigned(MainImage) then
  begin
    SavePath := IncludeTrailingPathDelimiter(ImgListView.Root);
    try
      if not DirectoryExists(SavePath + 'Flipped') then
        CreateDir(SavePath + 'Flipped');
      i := LastDelimiter('.' + PathDelim + DriveDelim, ImgListView.Selected.Caption);
      if ((i=0)  or  (ImgListView.Selected.Caption[i] <> '.')) then
        i := MaxInt;
      SaveImgName := ExtractFileName(Copy(ImgListView.Selected.Caption, 1, i-1));
      SaveImgExt := ExtractFileExt(ImgListView.Selected.Caption);
      SaveImage(MainCastleImage, SavePath + IncludeTrailingPathDelimiter('Flipped') + '(Flipped)_' + SaveImgName + SaveImgExt);
    except
      on E: Exception do
        ShowMessage('Error: ' + E.ClassName + #13#10 + E.Message);
    end;
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

procedure TMainForm.BtnSelectImageClick(Sender: TObject);
begin
  if SelectFirstImg.Execute then
  begin
    EnableCreateSpriteSheet;
    LoadImages(SelectFirstImg.FileName);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  NilAndFree;
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
  BtnFlipHorz.Enabled := True;
  BtnFlipVert.Enabled := True;
  BtnFlipBoth.Enabled := True;
  BtnSaveFlip.Enabled := True;
  ImgListView.Enabled := True;
end;

procedure TMainForm.DisableCreateSpriteSheet;
begin
  BtnSelectImage.Enabled := False;
  EdPadding.Enabled := False;
  EdNumCols.Enabled := False;
  EdSpriteSheetName.Enabled := False;
  BtnExecute.Enabled := False;
  BtnFlipHorz.Enabled := False;
  BtnFlipVert.Enabled := False;
  BtnFlipBoth.Enabled := False;
  BtnSaveFlip.Enabled := False;
  ImgListView.Enabled := False;
end;

procedure TMainForm.EnableSpriteSheetPreview;
begin
  EdPreviewFrameCount.Enabled := True;
  EdPreviewNumCols.Enabled := True;
  EdPreviewHorzSizeFrame.Enabled := True;
  EdPreviewVertSizeFrame.Enabled := True;
  EdPreviewFramePerSec.Enabled := True;
  BtnPreview.Enabled := True;
  ImgListView.Enabled := True;
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
  ImgListView.Enabled := False;
end;

procedure TMainForm.ChangingTypeOfSpriteSheet;
begin
  ResetShowImage;
  ImgListView.Root := '';
  imglistview.Clear;
  ItemSel.Caption := '(none)';
  LblItemsNum.Caption := '0';
  ImgsSquare.Visible := False;
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
  FreeAndNil(MainImage);
  FreeAndNil(MainCastleImage);
  FreeAndNil(ResizeCastleImage);
  FreeAndNil(ListFileName);
  FreeAndNil(Imgs);
  FreeAndNil(Player);
  FreeAndNil(CreateScrollBar);
end;

end.


