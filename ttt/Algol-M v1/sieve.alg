BEGIN

COMMENT
  BYTE magazine's CLASSIC SIEVE OF ERATOSTHENES;

INTEGER O, LIMIT, I, K, COUNT, PRIME;
INTEGER ARRAY FLAGS[0:8190];
LIMIT := 8190;

FOR O := 1 STEP 1 UNTIL 10 DO
BEGIN
    % INITIALIZE TABLE %
    FOR I := 0 STEP 1 UNTIL LIMIT DO
        FLAGS[I] := 1;
    
    % SIEVE FOR PRIMES %
    COUNT := 0;
    FOR I := 0 STEP 1 UNTIL LIMIT DO
    BEGIN
        IF FLAGS[I] = 1 THEN
        BEGIN
            PRIME := I + I + 3;
            IF PRIME < LIMIT THEN % extra check required to elimiate runtime warning %
            BEGIN
                FOR K := (I + PRIME) STEP PRIME UNTIL LIMIT DO
                    FLAGS[K] := 0;
            END;
            COUNT := COUNT + 1;
        END;
    END;
END;

WRITE(COUNT, " PRIMES WERE FOUND.");

END
