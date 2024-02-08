program SimsDB;
{
  This web application is a modern update for the original web exports which
  the original "The Sims" game exported upon saving a household, meant to be
  uploaded to a static hosting provider such as most ISPs at the time to share
  your life stories with family and friends over the Internet.  I'm not really
  sure how much this was used or how many people actually knew of it's existence,
  but this is my little attempt at preserving that and making it available and
  working in a modern web browser.  The thing is, if you tried to open up those
  sites now in a modern browser, it just doesn't work anymore, so any site that
  used it is now broken.  Anyways, I hope this project is of a benefit to someone.
}
{$mode objfpc}

uses
  browserconsole, browserapp, JS, Classes, SysUtils, Web, webrouter, jsontable;

type

  TAppPage = (apNone, apHome, apMember, apLot);

  { TMyApplication }

  TMyApplication = class(TBrowserApplication)
  private
    FSimsDB: TJSONDatabase;
    FSimDB, FLotDB, FFamilyDB: TJSONTable;
    FPage: TAppPage;
    procedure ShowLayer(Id: string);
    procedure HideLayer(Id: string);
    procedure SetContent(Id, content: string);
    procedure HideAllLayers;
    procedure PadMembersTable;
    function ShowFamilyHome(aEvent: TJSMouseEvent): boolean;
    function ShowFamilyLot(aEvent: TJSMouseEvent): boolean;
    procedure FamilyHomeView(URl : String; aRoute : TRoute; Params: TStrings);
    procedure FamilyMemberView(URl : String; aRoute : TRoute; Params: TStrings);
    procedure AlbumView(URl : String; aRoute : TRoute; Params: TStrings);
    procedure AdjustCenterBox(width: integer; imgsrc: string);
    procedure ShowHouse(floor: integer);
    function Floor1Click(aEvent: TJSMouseEvent): boolean;
    function Floor2Click(aEvent: TJSMouseEvent): boolean;
    function Floor3Click(aEvent: TJSMouseEvent): boolean;
    function SeeHouse(aEvent: TJSMouseEvent): boolean;
    function SeeNeighborhood(aEvent: TJSMouseEvent): boolean;
    procedure DatabaseLoaded;
    function GetLifeStage(Id: integer): string;
    function GetSign(Id: integer): string;
    function GetFamilyDir: string;
  protected
    procedure doRun; override;
  end;

const
  ResetLayers: Array of string = ('ScrapbookView', 'TitleText', 'FamilyText',
                   'MembersBox', 'HouseThumb', 'HouseCaption',
                   'HouseCaptionShadow', 'SeeNeighborhoodLink', 'FamilyStats',
                   'LinksBar', 'PersonalStatsBox', 'SimView', 'BioView',
                   'StatsView', 'SkillsView', 'Description', 'FloorShadow',
                   'TheHouse', 'HouseShadow', 'HouseInfoView', 'HouseStatsView',
                   'FloorCaption');

  FamilyLayers: Array of string = ('FamilyText', 'MembersBox', 'HouseThumb',
                   'HouseCaption', 'HouseCaptionShadow', 'SeeNeighborhoodLink',
                   'FamilyStats', 'LinksBar');

  MemberLayers: Array of string = ('PersonalStatsBox', 'TitleText', 'SimView',
                   'BioView', 'StatsView', 'SkillsView', 'Description');

  HouseLayers: Array of string = ('FamilyText', 'FloorShadow', 'TheHouse',
                   'HouseShadow', 'HouseInfoView', 'HouseStatsView',
                   'FloorCaption');

procedure TMyApplication.ShowLayer(Id: string);
begin
  GetHTMLElement(Id).hidden:=False;
end;

procedure TMyApplication.HideLayer(Id: string);
begin
  GetHTMLElement(Id).hidden:=True;
end;

procedure TMyApplication.SetContent(Id, content: string);
begin
  GetHTMLElement(Id).innerHTML:=content;
end;

procedure TMyApplication.HideAllLayers;
var
  i: Integer;
begin
  for i:=0 to Length(ResetLayers)-1 do
    HideLayer(ResetLayers[i]);
  for i:=1 to 4 do
  begin
    HideLayer('FamilyMember'+IntToStr(i));
    HideLayer('FamilyMemberShadow'+IntToStr(i));
    HideLayer('FamilyMemberName'+IntToStr(i));
    if i < 4 then
    begin
      HideLayer('FloorNav'+IntToStr(i));
    end;
  end;
end;

procedure TMyApplication.PadMembersTable;
var
  tr: TJSHTMLTableRowElement;
  td: TJSHTMLTableCellElement;
  img: TJSHTMLImageElement;
  tbl: TJSHTMLTableElement;
begin
  tbl:=TJSHTMLTableElement(GetHTMLElement('MembersTable'));
  tr:=tbl.insertRow(0);
  td:=tr.insertCell(0);
  td.Attrs['bgcolor']:='#EEEEDD';
  img:=TJSHTMLImageElement.new;
  img.src:='s/border.gif';
  img.Attrs['vspace']:='0';
  img.Attrs['hspace']:='0';
  img.height:=100;
  img.width:=1;
  img.Attrs['border']:='0';
  img.Attrs['align']:='left';
  td.appendChild(img);
  td:=tr.insertCell(1);
  td.Attrs['bgcolor']:='#EEEEDD';
  td.Attrs['width']:='175';
  td.innerHTML:='<br/>';
  td:=tr.insertCell(2);
  td.Attrs['bgcolor']:='#EEEEDD';
  img:=TJSHTMLImageElement.new;
  img.src:='s/border.gif';
  img.Attrs['vspace']:='0';
  img.Attrs['hspace']:='0';
  img.height:=100;
  img.width:=1;
  img.Attrs['border']:='0';
  img.Attrs['align']:='left';
  td.appendChild(img);
end;

function TMyApplication.ShowFamilyHome(aEvent: TJSMouseEvent): boolean;
var
  i: integer;
begin
  if FPage <> apHome then
  begin
    Result:=True;
    Exit;
  end;
  HideAllLayers;
  AdjustCenterBox(365, 'familyhomepagetxt');
  for i:=0 to Length(FamilyLayers)-1 do
    ShowLayer(FamilyLayers[i]);
  for i:=1 to FFamilyDB.Ints['Members'] do
  begin
    ShowLayer('FamilyMember'+IntToStr(i));
    ShowLayer('FamilyMemberShadow'+IntToStr(i));
    ShowLayer('FamilyMemberName'+IntToStr(i));
  end;
  Result:=False;
end;

function TMyApplication.ShowFamilyLot(aEvent: TJSMouseEvent): boolean;
var
  i: Integer;
begin
  //FPage:=apLot;
  FLotDB.Filter:='ID='+IntToStr(FFamilyDB.Ints['Lot']);
  if FLotDB.DataSet.EOF then
  begin
    window.alert('Invalid!');
    Exit;
  end;
  TJSHTMLImageElement(GetHTMLElement('HouseImage')).src:=GetFamilyDir+'house-exterior.jpg';
  with FLotDB do
  begin
    document.title:='Number '+Strings['Name']+':';
    SetContent('FloorText', Strings['Name']+': Exterior');
    SetContent('HouseValue', IntToStr(Ints['Value']));
    SetContent('HouseSize', IntToStr(Ints['Size']));
    SetContent('HouseFloors', IntToStr(Ints['Floors']));
    SetContent('HouseBaths', IntToStr(Ints['Baths']));
    SetContent('HouseBeds', IntToStr(Ints['Beds']));
    SetContent('StatSize', IntToStr(Ints['StatSize']));
    SetContent('StatFurnish', IntToStr(Ints['Furnish']));
    SetContent('StatYard', IntToStr(Ints['Yard']));
    SetContent('StatUpKeep', IntToStr(Ints['Upkeep']));
    SetContent('StatLayout', IntToStr(Ints['Layout']));
  end;
  HideAllLayers;
  for i:=0 to Length(HouseLayers)-1 do
    ShowLayer(HouseLayers[i]);
  for i:=1 to 3 do
    ShowLayer('FloorNav'+IntToStr(i));
  AdjustCenterBox(571, 'familyhometxt');
  GetHTMLElement('Level1Btn').onclick:=@Floor1Click;
  GetHTMLElement('Level2Btn').onclick:=@Floor2Click;
  GetHTMLElement('Level3Btn').onclick:=@Floor3Click;
end;

procedure TMyApplication.FamilyHomeView(URl: String; aRoute: TRoute;
  Params: TStrings);
var
  i: integer;
  tbl: TJSHTMLTableElement;
  lnk: string;
begin
  FFamilyDB.Filter:='Id='+Params.Values['FAMILY'];
  if FFamilyDB.DataSet.EOF then
  begin
    window.alert('Invalid!');
    Exit;
  end;
  TJSHTMLImageElement(GetHTMLElement('HousePhoto')).src:=GetFamilyDir+'house-thumb.jpg';
  with FFamilyDB do
  begin
    SetContent('FamilyName', '<center>'+Strings['Name']+'</center>');
    document.title:='The '+Strings['Name']+' Family Home Page';
    SetContent('CashBalance', '$'+IntToStr(Ints['Cash']));
    SetContent('Days', IntToStr(Ints['Days']));
    SetContent('Members', IntToStr(Ints['Members']));
    SetContent('Friends', IntToStr(Ints['Friends']));
  end;
  tbl:=TJSHTMLTableElement(GetHTMLElement('MembersTable'));
  if tbl.rows.length > 1 then
  begin
    repeat
      tbl.deleteRow(tbl.rows.length-1);
    until tbl.rows.length = 1;
  end;
  lnk:='#/Family/'+Params.Values['FAMILY']+'/Member/';
  for i:=1 to FFamilyDB.Ints['Members'] do
  begin
    FSimDB.Filter:='ID='+IntToStr(FFamilyDB.Ints['Member'+IntToStr(i)]);
    SetContent('MemberName'+IntToStr(i), '<center>'+FSimDB.Strings['Name']+'</center>');
    TJSHTMLImageElement(GetHTMLElement('MemberFace'+IntToStr(i))).src:=GetFamilyDir+'family'+IntToStr(i)+'_face.jpg';
    TJSHTMLAnchorElement(GetHTMLElement('Member'+IntToStr(i)+'Link1')).href:=lnk+IntToStr(i);
    TJSHTMLAnchorElement(GetHTMLElement('Member'+IntToStr(i)+'Link2')).href:=lnk+IntToStr(i);
    PadMembersTable;
  end;
  FPage:=apHome;
  ShowFamilyHome(Nil);
  FPage:=apHome;
  GetHTMLElement('FamilyLink').onclick:=@ShowFamilyHome;
  TJSHTMLAnchorElement(GetHTMLElement('FamilyLink')).href:='#/Family/'+Params.Values['FAMILY'];
  GetHTMLElement('LotLink').onclick:=@ShowFamilyLot;
end;

procedure TMyApplication.FamilyMemberView(URl: String; aRoute: TRoute;
  Params: TStrings);
var
  i: Integer;
begin
  FFamilyDB.Filter:='ID='+Params.Values['FAMILY'];
  if FFamilyDB.DataSet.EOF then
  begin
    window.alert('Invalid!');
    Exit;
  end;
  FSimDB.Filter:='ID='+IntToStr(FFamilyDB.Ints['Member'+Params.Values['ID']]);
  if FSimDB.DataSet.EOF then
  begin
    window.alert('Invalid!');
    Exit;
  end;
  with FSimDB do
  begin
    SetContent('SimName', '<center>'+Strings['Name']+'</center>');
    document.title:=Strings['Name']+'''s Biography';
    SetContent('SimBio', Strings['Bio']);
    SetContent('SimGender', Strings['Gender']);
    SetContent('SimAge', GetLifeStage(Ints['Stage']));
    SetContent('SimSign', GetSign(Ints['Sign']));
    SetContent('SimCareer', Strings['Career']);
    SetContent('SimJob', Strings['Job']);
    SetContent('SimJobPerform', IntToStr(Ints['Performance']));
    SetContent('SimSalary', '$'+IntToStr(Ints['Salary']));
    SetContent('SimNeat', IntToStr(Ints['Neat']));
    SetContent('SimOutgoing', IntToStr(Ints['Outgoing']));
    SetContent('SimActive', IntToStr(Ints['Active']));
    SetContent('SimPlayful', IntToStr(Ints['Playful']));
    SetContent('SimNice', IntToStr(Ints['Nice']));
    SetContent('SimCooking', IntToStr(Ints['Cooking']));
    SetContent('SimMechanical', IntToStr(Ints['Mechanical']));
    SetContent('SimCharisma', IntToStr(Ints['Charisma']));
    SetContent('SimBody', IntToStr(Ints['Body']));
    SetContent('SimLogic', IntToStr(Ints['Logic']));
    SetContent('SimCreativity', IntToStr(Ints['Creativity']));
  end;
  HideAllLayers;
  TJSHTMLImageElement(GetHTMLElement('SimPhoto')).src:=GetFamilyDir+'family'+Params.Values['ID']+'_full.jpg';
  for i:=0 to Length(MemberLayers)-1 do
    ShowLayer(MemberLayers[i]);
  AdjustCenterBox(365, 'familymembertxt');
  FPage:=apMember;
end;

procedure TMyApplication.AlbumView(URl: String; aRoute: TRoute; Params: TStrings
  );
begin
  HideLayer('CenterBox');
  HideAllLayers;
  ShowLayer('ScrapbookView');
end;

procedure TMyApplication.AdjustCenterBox(width: integer; imgsrc: string);
var
  cnt, e: TJSHTMLElement;
  img: TJSHTMLImageElement;
  tbl: TJSHTMLTableElement;
  row: TJSHTMLTableRowElement;
begin
  ShowLayer('CenterBox');
  cnt:=GetHTMLElement('CenterBox');
  cnt.style.setProperty('width', IntToStr(width)+'px');
  img:=TJSHTMLImageElement(GetHTMLElement('TitleImage'));
  img.src:='s/'+imgsrc+'.jpg';
  img.width:=width-1;
  tbl:=TJSHTMLTableElement(cnt.children.Items[0]);
  tbl.width:=IntToStr(width);
  tbl:=TJSHTMLTableElement(cnt.children.Items[1]);
  tbl.width:=IntToStr(width);
  row:=TJSHTMLTableRowElement(tbl.rows.Items[0]);
  {TJSHTMLImageElement(row.cells.Items[0].childNodes.item(0)).height:=450;}
  TJSHTMLTableCellElement(row.cells.Items[1]).width:=IntToStr(width);
  {TJSHTMLImageElement(row.cells.Items[2].childNodes.item(0)).height:=450;}
  tbl:=TJSHTMLTableElement(cnt.children.Items[2]);
  tbl.width:=IntToStr(width);
  row:=TJSHTMLTableRowElement(tbl.rows.Items[0]);
  TJSHTMLImageElement(row.cells.Items[0].childNodes.item(0)).width:=width;
end;

procedure TMyApplication.ShowHouse(floor: integer);
var
  imgsrc: string;
begin
  imgsrc:='house-floor'+IntToStr(floor);
  if floor = 1 then
    SetContent('FloorText', FLotDB.Strings['Name']+': First Floor Interior')
  else if floor = 2 then
    SetContent('FloorText', FLotDB.Strings['Name']+': Second Floor Interior')
  else if floor = 3 then
  begin
    SetContent('FloorText', FLotDB.Strings['Name']+': Exterior');
    imgsrc:='house-exterior';
  end;
  TJSHTMLImageElement(GetHTMLElement('HouseImage')).src:=GetFamilyDir+imgsrc+'.jpg';
end;

function TMyApplication.Floor1Click(aEvent: TJSMouseEvent): boolean;
begin
  ShowHouse(1);
  Result:=False;
end;

function TMyApplication.Floor2Click(aEvent: TJSMouseEvent): boolean;
begin
  ShowHouse(2);
  Result:=False;
end;

function TMyApplication.Floor3Click(aEvent: TJSMouseEvent): boolean;
begin
  ShowHouse(3);
  Result:=False;
end;

function TMyApplication.SeeHouse(aEvent: TJSMouseEvent): boolean;
begin
  HideLayer('HouseIconLayer');
  HideLayer('SeeHouseLink');
  ShowLayer('HouseThumb');
  ShowLayer('SeeNeighborhoodLink');
end;

function TMyApplication.SeeNeighborhood(aEvent: TJSMouseEvent): boolean;
begin
  ShowLayer('HouseIconLayer');
  HideLayer('SeeNeighborhoodLink');
  ShowLayer('SeeHouseLink');
  HideLayer('HouseThumb');
end;

procedure TMyApplication.DatabaseLoaded;
begin
  FSimDB:=FSimsDB.Table['sims'];
  FLotDB:=FSimsDB.Table['lots'];
  FFamilyDB:=FSimsDB.Table['families'];
  ShowLayer('NavBar');
  if Router.RouteFromURL = '' then
    Router.Push('/Family/1');
end;

function TMyApplication.GetLifeStage(Id: integer): string;
begin
  case Id of
    1: Result:='Kid';
    2: Result:='Adult';
  end;
end;

function TMyApplication.GetSign(Id: integer): string;
begin
  case Id of
    1: Result:='Leo';
    2: Result:='Capracorn';
  end;
end;

function TMyApplication.GetFamilyDir: string;
begin
  Result:='Families/'+FFamilyDB.Strings['Name']+'/';
end;

procedure TMyApplication.doRun;
begin
  FPage:=apLot;
  GetHTMLElement('SeeHouseLnk').onclick:=@SeeHouse;
  GetHTMLElement('SeeNeighborhoodLnk').onclick:=@SeeNeighborhood;
  Router.InitHistory(hkHash);
  Router.RegisterRoute('/Family/:FAMILY', @FamilyHomeView);
  Router.RegisterRoute('/Family/:FAMILY/Member/:ID', @FamilyMemberView);
  Router.RegisterRoute('/ScrapBook/:FAMILY', @AlbumView);
  FSimsDB:=TJSONDatabase.Create(Self);
  FSimsDB.Datafile:='SimsDB';
  FSimsDB.OnSuccess:=@DatabaseLoaded;
  FSimsDB.Active:=True;
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
