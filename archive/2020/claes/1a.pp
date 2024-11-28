program AOC1a;

uses
 Sysutils;

const
  sFileName = '1a.input';

var
    fIn : Text;
    sLine : String;
    entries : Array[0..1000] OF Integer;
    iIndex, iErr, a, b : Integer;

BEGIN
    iIndex := 0;
    write('Reading file..');
    assign(fIn, sFileName);
    reset(fIn);

    REPEAT
        readln(fIn, sLine);
        val(sLine, entries[iIndex], iErr);
        inc(iIndex);
    UNTIL Eof(fIn);

    write(iIndex);
    writeln(' lines read.');

    a := 0;
    b := 0;

    REPEAT
        inc(b);
        IF (b>= iIndex) THEN BEGIN
            inc(a);
            b := a;
        END;
    UNTIL (entries[a] + entries[b] = 2020) OR (a >= iIndex);

    writeln(Format( 'Result: IndexA=%d, IndexB=%d', [a, b]));
    writeln('A: ', entries[a]);
    writeln('B: ', entries[b]);
    writeln('Result: ', entries[a] * entries[b]);

    close(fIn);
END.