program problem72
  implicit none
  integer, parameter    :: m=664579,limit=1000000
  integer, dimension(m) :: primes
  integer               :: i
  integer*16            :: answer

  open(33,file='./data/problem70.dat')
  read(33,*) primes

  answer=0
  do i=2,limit
     answer=answer+phi(i)
  end do

  print *, answer

contains

  function phi(n)
    implicit none
    integer, intent(in) :: n
    integer             :: p,phi,i,rest

    phi=n
    rest=n
    do i=1,n
       p=primes(i)
       if (mod(rest,p)==0) then
          phi=(phi/p)*(p-1)
          do while (mod(rest,p)==0)
             rest=rest/p
          end do
       end if

       ! Important: if the current prime is more than the square root
       ! of what is left, then what is left must be the last prime
       ! factor.
       if (p>sqrt(real(rest))) then
          if (rest>1) phi=(phi/rest)*(rest-1)
          exit
       end if
    end do

  end function phi

end program problem72
