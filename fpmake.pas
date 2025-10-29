program fpmake;

{$MODESWITCH EXCEPTIONS}
{$MACRO ON}
{$H+}

uses fpmkunit, strutils, sysutils;

const
    Opts: string =
        '-Sa -Si -Sm -Sc -Sx -Fusrc -Fisrc/inc';
        // assertions
        //     inlines
        //         macros
        //             C operators
        //                exceptions

var
    P: TPackage;

procedure AddProgramOrUnit(const name: shortstring; isAnUnit: boolean);
var craftedName: string;
begin
    if isAnUnit then
        craftedName := '../src/' + name + '.pas'
    else
        craftedName := Format('../examples/%0:s/%0:s.lpr', [ name ]);

    if not FileExists(P.Directory + craftedName) then
        raise Exception.Create('Source file not found: ' + P.Directory + craftedName);

    if isAnUnit then
        p.Targets.AddUnit(craftedName)
    else
        p.Targets.AddProgram(craftedName);
end;

var
    Splits: array of string;
    n: integer;

begin
    AddCustomFpmakeCommandlineOption(
        'build-mode', 'What to build: units for ones in src/, examples for one in examples.');
    P := Installer.AddPackage('cli-fp');

    with P do begin
        NeedLibC := true; // some UNIX units, and maybe more
        Directory := 'example-bin/';
        HomepageURL := 'https://github.com/ikelaiah/cli-fp';
        DownloadURL := HomepageURL + '/releases';
        Author := 'ikelaiah';
        License := 'MIT';
        DescriptionFile := 'README.md';
    end;

    Splits := strutils.SplitString(Opts, ' ');
    for n := low(Splits) to high(Splits) do
        Defaults.Options.Append(Splits[n]);

    case GetCustomFpmakeCommandlineOptionValue('build-mode') of
        'units': begin
            {$I src/fpmake.inc}
        end;
        'examples': begin
            {$I examples/fpmake.inc}
        end;
    else
        raise Exception.Create('Unknown buildmode. Must be either units or examples.');
    end;

    Installer.Run;
end.