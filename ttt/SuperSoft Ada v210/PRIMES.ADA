--pragma print(a_code);
pragma optimize(time);
procedure prime_factors is

k : integer;                            -- index into flag array
prime : integer;                        -- prime number
count : integer;                        -- number of primes found
size : constant integer := 8190;        -- largest index
flags : array (0..size) of boolean;

-- Address specifications are used here to speed up the execution time
-- of the program.  They allow the Ada compiler to use a direct address
-- rather than looking up the address of the variable on the run-time stack.
-- Placing the large array at 20,000 assumes the existence of a 48K machine.

for k use at 128;               -- 
for prime use at 130;           -- put these in the default disk buffer
for count use at 132;           --
for size use at 134;            --

for flags use at 20000;         -- put the big array in memory somewhere...

begin
        put("10 iterations");new_line;

-- do this routine ten times

        for iter in 1..10 loop
                count := 0;

                -- initialize array of flags to true

                for i in 0..size loop
                        flags(i) := true;
                end loop;

                -- loop thru array, looking for prime numbers

                for i in 0..size loop
                        if flags(i) then
                                prime := i + i + 3;
                                k := i + prime;

                                -- loop to kill multiples

                                while k <= size loop
                                        flags(k) := false;
                                        k := k + prime;
                                end loop;
                                count := count + 1;
                        end if;
                end loop;
        end loop;
        put(count);
        put(" primes");
        new_line;

end prime_factors;
