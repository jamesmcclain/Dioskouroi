program problem77
  implicit none
  integer, parameter     :: ms=100,ps=664579,limit=5000
  integer, dimension(ps) :: primes
  integer, dimension(ms) :: memo
  integer                :: i,a

  open(33,file='./data/problem70.dat')
  read(33,*) primes

  ! https://oeis.org/A000607
  do i=1,ms
     memo(i)=-1
  end do
  memo(1:10)=(/ 0, 1, 1, 1, 2, 2, 3, 3, 4, 5 /)

  i=1
  a=1
  do while(a<=limit)
     i=i+1
     a=a000607(i)
  end do
  print *, i

contains

  ! https://oeis.org/A000607
  ! a(n) = (1/n)*Sum_{k=1..n} A008472(k)*a(n-k)
  recursive function a000607(n) result(r)
    implicit none
    integer, intent(in) :: n
    integer             :: k,r

    if (n<0) then
       r=1
       return
    else if (n==0) then
       r=1
       return
    else
       if (memo(n) /= -1) then
          r=memo(n)
          return
       else
          r=0
          do k=1,n
             r=r+(a008472(k)*a000607(n-k))
          end do
          r=r/n
          if (n<=ms) memo(n)=r
          return
       end if
    end if
    
  end function a000607
  
  ! https://oeis.org/A008472
  pure function a008472(n)
    implicit none
    integer, intent(in) :: n
    integer             :: p,a008472,i,rest

    a008472=0
    rest=n
    do i=1,n
       p=primes(i)
       if (mod(rest,p)==0) then
          a008472=a008472+p
          do while (mod(rest,p)==0)
             rest=rest/p
          end do
       end if

       ! Important: if the current prime is more than the square root
       ! of what is left, then what is left must be the last prime
       ! factor.
       if (p>sqrt(real(rest))) then
          if (rest>1) a008472=a008472+rest
          exit
       end if
    end do

  end function a008472

end program problem77
