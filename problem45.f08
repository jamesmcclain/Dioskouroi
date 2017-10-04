program problem45
  implicit none
  integer*8 :: n,temp

  do n=286,huge(n)
     temp=T(n)
     if (is_P(temp) .and. is_H(temp)) exit
  end do

  print *, n, T(n)

contains

  pure function T(n)
    implicit none
    integer*8, intent(in) :: n
    integer               :: T
    T=n*(n+1)/2
  end function T

  pure function is_P(P)
    implicit none
    integer*8, intent(in) :: P
    double precision      :: n
    logical               :: is_P
    n = (sqrt(dble(24*P + 1)) + 1)/6
    is_P = (whole(n))
  end function is_P

  pure function is_H(H)
    implicit none
    integer*8, intent(in) :: H
    double precision      :: n
    logical               :: is_H
    n = (sqrt(dble(8*H + 1)) + 1)/4
    is_H = (whole(n))
  end function is_H

  pure function whole(x)
    implicit none
    double precision, intent(in) :: x
    logical                      :: whole
    whole = (mod(x,1.0) == 0.0)
  end function whole

end program problem45
