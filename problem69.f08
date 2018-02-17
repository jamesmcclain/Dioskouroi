program problem69
  implicit none
  integer, parameter    :: n=78498,limit=1000000
  integer, dimension(n) :: primes
  integer               :: i,temp

  ! https://primes.utm.edu/lists/small/100000.txt
  open(33,file='./data/problem69.dat')
  read(33,*) primes

  ! https://en.wikipedia.org/wiki/Euler%27s_totient_function
  ! $\frac{n}{\phi(n)} = \prod_{p|n}(\frac{p}{p-1})$
  ! Factors with powers greater than one increase n but not n/phi(n).
  ! Small factors are preferable to large ones, so greedily multiply.
  temp=1
  do i=1,n
     temp=temp*primes(i)
     if (temp>limit) then
        temp=temp/primes(i)
        go to 100
     end if
  end do

100 continue
  print *, temp

end program problem69
