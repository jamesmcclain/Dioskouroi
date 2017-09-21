program problem37
  use euler
  implicit none
  integer, parameter :: limit=11
  integer            :: count=0,answer=0
  integer*8          :: i

  i=11
  do while(count<limit)
     if (left_prime(i) .and. right_prime(i)) then
        print *, i
        count=count+1
        answer=answer+i
     end if
     i=i+2
  end do

  print *, answer

contains

  pure function left_prime(n)
    implicit none
    integer*8, intent(in) :: n
    integer*8             :: temp
    logical               :: left_prime

    left_prime=.true.
    temp=n
    do while(temp>0)
       if (.not. is_prime(temp)) then
          left_prime=.false.
          exit
       end if
       temp=mod(temp,10**(number_length(temp)-1))
    end do

  end function left_prime

  pure function right_prime(n)
    implicit none
    integer*8, intent(in) :: n
    integer*8             :: temp
    logical               :: right_prime

    right_prime=.true.
    temp=n
    do while(temp>0)
       if (.not. is_prime(temp)) then
          right_prime=.false.
          exit
       end if
       temp=temp/10
    end do

  end function right_prime

end program problem37
