program problem26
  implicit none
  integer :: i,temp,answer=-1,best=-1

  do i=2,1000
     temp=period(i)
     if (temp>best) then
        best=temp
        answer=i
     end if
  end do

  print *, answer

contains
  pure function period(n)
    implicit none
    integer, intent(in)      :: n
    integer, parameter       :: size=1000000
    integer, dimension(size) :: quotients,remainders
    integer                  :: period,i,j

    quotients(1)=0
    remainders(1)=1

    ! Generate sequence of (quotient,remainder) pairs.  A cycle occurs
    ! when and only when a (quotient,remainder) pair is repeated.
    do i=2,size
       quotients(i)=(remainders(i-1)*10)/n
       remainders(i)=mod(remainders(i-1)*10,n)
       ! Look for a repeated pair
       do j=1,i-1
          if (quotients(i)==quotients(j) .and. remainders(i)==remainders(j)) then
             period=i-j
             go to 1
          end if
       end do
    end do
1   continue

  end function period

end program problem26
