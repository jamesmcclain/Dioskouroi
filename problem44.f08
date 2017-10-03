program problem44
  implicit none
  integer, parameter :: limit=1000
  integer            :: answer,a,b,n,j
  double precision   :: temp1,temp2

  do a=1,limit
     do b=1,limit
        if (b == a) cycle
        temp1=solution_n(a,b)
        temp2=solution_nj(a,b)
        if (temp1 > 0 .and. whole(temp1) .and. whole(temp2)) then
           n=int(temp1)
           j=n-int(temp2)
           print *, a,b,n,j,P(n-b),P(n+j)-P(n)
        end if
     end do
  end do

  print *, answer

contains

  ! Solution in n to  2P_{n} = P_{n+a} - P_{n-b}
  pure function solution_n(a,b)
    implicit none
    integer, intent(in) :: a,b
    double precision    :: solution_n
    solution_n = dble(-3*a*a + a - 3*b*b - b)/(6*(a-b))
  end function solution_n

  pure function solution_nj(a,b,j)
    implicit none
    integer, intent(in) :: a,b
    double precision    :: D,solution_nj
    D = sqrt(dble(27*a*a + 18*a*b + 36*a*j - 9*b*b + 36*b*j + 1))
    solution_nj = (D + 3*a + 3*b + 6*j + 1)/6
  end function solution_nj

  pure function P(n)
    implicit none
    integer, intent(in) :: n
    integer             :: P
    P=n*(3*n-1)/2
  end function P

  pure function whole(x)
    implicit none
    double precision, intent(in) :: x
    logical                      :: whole
    whole = (mod(x,1.0) == 0.0)
  end function whole

end program problem44
