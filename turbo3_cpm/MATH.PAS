program math;

var a, b, c, d, e : Integer;

begin

    a := 0;
    b := -30;
    c := 50;
    Writeln( 'a, b, c: ', a, ', ', b, ', ', c );
    Writeln( 'b + c should be 20: ', b + c );
    Writeln( 'b * 100 should be -3000: ', b * 100 );
    Writeln( 'c * 10000 shoud overflow: ', c * 10000 );
end.
