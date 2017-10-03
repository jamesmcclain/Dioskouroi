program problem44
  implicit none
  integer, parameter :: limit=100
  integer            :: answer,j,k,n
  double precision   :: temp

  do j=1,limit
     do k=1,limit
        temp=solution_in_n(j,k)
        if (whole(temp)) then
           n=int(temp)
           temp=solution_in_k(n,j)
           if (whole(temp)) then
              print *, n,j,int(temp)
           end if
        end if
     end do
  end do

  print *, answer

contains

  ! Solution in n to  P_{n+j} - P_{n} = P_{n-k}.
  pure function solution_in_n(j,k)
    implicit none
    integer, intent(in) :: j,k
    double precision    :: D,solution_in_n

    D = sqrt(dble(72*j*j + 72*j*k + 1))
    solution_in_n = (D + 6*(j+k) +1)/6
  end function solution_in_n

  ! Solution in k to P_{n} + P_{n+j} = P_{n+k}.
  pure function solution_in_k(n,j)
    implicit none
    integer, intent(in) :: n,j
    double precision D,solution_in_k

    D = sqrt(dble(36*j*j + 12*j*(6*n-1) + 72*n*n - 24*n + 1))
    solution_in_k = (D - 6*n + 1)/6
  end function solution_in_k

  pure function whole(x)
    implicit none
    double precision, intent(in) :: x
    logical                      :: whole
    whole = (mod(x,1.0) == 0.0)
  end function whole

end program problem44
