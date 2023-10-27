unit testconstants;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  { The relative path of the TestDataDirectory }
  TestDataDirectory:String = 'testdata';
  { The number of Song files in the test data directory which will be checked against }
  NumberOfSongFiles:Integer = 5;

implementation

end.

