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



implementation

begin
  clbLightest := RGBToColor(205,235,235); //$CDEBEB;
  clbLight := RGBToColor(167,222,222); //$A7DEDE;
  clbMedium := RGBToColor(130,208,208); //$82D0D0;
  clbDark := RGBToColor(99,195,195); //$63C3C3;
  clbDarkest := RGBToColor(73,184,184); //$49B8B8;
end.

