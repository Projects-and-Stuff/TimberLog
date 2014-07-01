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

