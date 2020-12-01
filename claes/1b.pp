program AOC1a;

uses
 Sysutils;

const
  sFileName = '1a.input';

var
    fIn : Text;
    sLine : String;
    entries : Array[0..1000] OF Integer;
    iIndex, iErr, a, b, c : Integer;

BEGIN
    iIndex := 0;
    write('Reading file..');
    assign(fIn, sFileName);
    reset(fIn);

    REPEAT
        readln(fIn, sLine);
        val(sLine, entries[iIndex], iErr);
        inc(iIndex);
    UNTIL eof(fIn);

    write(iIndex);
    writeln(' lines read.');

    a := 0;
    b := 1;
    c := 1;

    REPEAT
        inc(c);

        IF (c >= iIndex) THEN BEGIN
            inc(b);
            c := b;
            IF (b >= iIndex) THEN BEGIN
                inc(a);
                b := a;
            END;
        END;
    UNTIL (entries[a] + entries[b] + entries[c] = 2020) OR (a >= (iIndex - 1));

    writeln(Format( 'Result: IndexA=%d, IndexB=%d, IndexC=%d', [a, b, c]));
    writeln('A: ', entries[a]);
    writeln('B: ', entries[b]);
    writeln('C: ', entries[c]);
    writeln('Result: ', entries[a] * entries[b] * entries[c]);

    close(fIn);
END.