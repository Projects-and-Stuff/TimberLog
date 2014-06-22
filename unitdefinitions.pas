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


  clbLightest := RGBToColor(255,231,244);
  clbLight := RGBToColor(238,123,188);
  clbMedium := RGBToColor(225,  0,127);
  clbDark := RGBToColor(165,  0, 93);
  clbDarkest := RGBToColor(120,  0, 68);


  clbBGDefault := clbLightest;
  clbBGMouseOver := clbLight;
  clbBGClicked := clbMedium;
  clbVividTextDefault := clbMedium;
  clbVividTextMouseOver := clbDark;
  clbVividTextClicked := clbDarkest;

end.

