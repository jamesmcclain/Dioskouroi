program problem44
  implicit none
  integer :: answer,j,k,Pj,Pk,D

  answer=huge(answer)

  do k=2,huge(k)
     Pk=P(k)
     if (Pk - P(k-1) > answer .or. Pk < 0) exit ! already too large, short circuit
     do j=k-1,1,-1
        Pj=P(j)
        D = Pk-Pj
        if (D > answer .or. Pj < 0) exit ! already too large, short circuit
        if (is_P(Pj + Pk) .and. is_P(D)) then
           print *, j,k,Pj,Pk,D
           if (D<answer) then
              answer=D
           end if
        end if
     end do
  end do

  print *, answer

contains

  pure function P(n)
    implicit none
    integer, intent(in) :: n
    integer             :: P
    P=n*(3*n-1)/2
  end function P

  pure function is_P(x)
    implicit none
    integer, intent(in) :: x
    double precision    :: n
    logical             :: is_P
    n = (sqrt(dble(24*x + 1)) + 1)/6
    is_P = (whole(n))
  end function is_P

  pure function whole(x)
    implicit none
    double precision, intent(in) :: x
    logical                      :: whole
    whole = (mod(x,1.0) == 0.0)
  end function whole

end program problem44
