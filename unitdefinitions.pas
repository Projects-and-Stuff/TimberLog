unit unitDefinitions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

var
  // Use the 'mono' on following site to help with scheme:
  // http://colorschemedesigner.com/
  // http://paletton.com/
  clbLightest : TColor;
  clbLight : TColor;
  clbMedium : TColor;
  clbDark : TColor;
  clbDarkest : TColor;

  clbBGDefault : TColor;
  clbBGMouseOver : TColor;
  clbBGClicked : TColor;
  clbVividTextDefault : TColor;
  clbVividTextMouseOver : TColor;
  clbVividTextClicked : TColor;

const
  RecHeader = '>TimberLog<';
  RecFooter = '>TimberLog<';

  blockLogbook_Type = '>>>*Logbook_Type*<<<';
  blockDescription = '>>>*Description*<<<';
  blockSettings = '>>>*Settings*<<<';
  blockDefault_Categories = '>>>*Default_Categories*<<<';
  blockDefault_Text = '>>>*Default_Text*<<<';
  blockOne_Line = '>>>*One_Line*<<<';
  blockMulti_Line = '>>>*Multi_Line*<<<';

  AppId = '277843001';


implementation

begin
  // Blue
  clbLightest := RGBToColor(205,235,235);
  clbLight := RGBToColor(167,222,222);
  clbMedium := RGBToColor(130,208,208);
  clbDark := RGBToColor( 99,195,195);
  clbDarkest := RGBToColor( 73,184,184);


  clbBGDefault := clbLightest;
  clbBGMouseOver := clbLight;
  clbBGClicked := clbMedium;
  clbVividTextDefault := clbMedium;
  clbVividTextMouseOver := clbDark;
  clbVividTextClicked := clbDarkest;

end.

