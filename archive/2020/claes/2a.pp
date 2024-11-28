program AOC1a;

uses
 Sysutils, AOCUtils;

const
  sFileName = '2.input';

var
    entries : Array OF String;
    iIndex, iValidPasswords : Integer;

FUNCTION isValidPasswordFormat(line : String) : BOOLEAN;
var
    minLength, maxLength, res, iIndex, iCharCount : Integer;
    validationChar : char;

BEGIN
    val(copy(line, 1, pos('-', line) - 1), minLength, res);
    val(Trim(copy(line, pos('-', line) + 1, 2)), maxLength, res);

    validationChar := line[pos(':', line) - 1];

    iIndex := pos(':', line) + 1;
    iCharCount := 0;

    REPEAT
        IF line[iIndex] = validationChar THEN
            inc(iCharCount);
        inc(iIndex)
    UNTIL iIndex > Length(line);

    writeln(iCharCount);
    IF (iCharCount >= minLength) AND (iCharCount<= maxLength) THEN
        isValidPasswordFormat := TRUE
    ELSE
        isValidPasswordFormat := FALSE;
END;

BEGIN
    iIndex := 0;
    iValidPasswords := 0;
    entries := readFileAsStrings(sFileName);

    REPEAT
        IF isValidPasswordFormat(entries[iIndex]) THEN
            inc(iValidPasswords);
        inc( iIndex);
    UNTIL iIndex >= Length(entries);

    write(iValidPasswords);
    writeln(' valid passwords found');
END.