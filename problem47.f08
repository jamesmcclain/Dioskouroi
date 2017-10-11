program problem47
  use euler
  implicit none
  integer, parameter             :: window=4,maxprime=1000000
  integer*8                      :: i,j,k,distinct,nprimes
  integer*8, dimension(maxprime) :: primes

  nprimes=1
  do i=2,maxprime
     if (is_prime(i)) then
        primes(nprimes)=i
        nprimes=nprimes+1
     end if
  end do

  do i=1,huge(i) ! start of window of numbers
     do j=i,i+window-1 ! for each number in the window
        distinct=0
        do k=1,nprimes
           if (primes(k) > j) exit
           if (mod(j,primes(k)) == 0) distinct=distinct+1
           if (distinct > window) go to 1
        end do
        if (distinct < window) go to 1
     end do
     exit ! made it through all of the checks, answer found
1 end do

  print *, i

end program problem47
