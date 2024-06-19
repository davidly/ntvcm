C Eratosthenes Sieve from BYTE magazine
      program sieve
      logical flags( 8191 )
      integer*2 i, prime, k, count
      integer*1 iter

      write( 1, 50 )
 50   format( ' 10 iterations' )
      do 92 iter = 1, 10
      count = 0
      do 10 i = 0, 8190
 10   flags( i ) = .true.
      do 91 i = 0, 8190
      if ( flags( i ) .eq. .false. ) goto 91
      prime = i + i + 3
      k = i + prime
 20   if ( k .gt. 8190 ) goto 90
      flags( k ) = .false.
      k = k + prime
      goto 20
 90   count = count + 1
 91   continue
 92   continue
      write( 1, 200 ) count
 200  format( 1X, I6, ' primes' )
      stop
 100  format( 1X, I6 )
      end
