unit CommandLineOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Lyrics;

procedure PrintHelp;
//procedure convertSong(inputFile: String);

implementation

procedure PrintHelp;
begin
  writeLn('Cantara Song Presentation Software');
  writeLn('Run without parameters to open graphical user interface.');
  writeLn('');
  writeLn('Supported Options');
  writeLn('');
  writeLn('--convert <songfile>');
  writeLn('    Converts a songfile into the song format');
end;

end.
