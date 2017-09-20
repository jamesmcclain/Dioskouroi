program problem35
  use euler
  implicit none
  integer :: i,answer=1

  do i=3,1000000,2
     if (is_circular_prime(int8(i))) then
        answer=answer+1
     end if
  end do

  print *, answer

contains
  pure function is_circular_prime(n)
    implicit none
    integer*8, intent(in) :: n
    integer*8             :: temp
    integer               :: i
    logical               :: is_circular_prime

    is_circular_prime=.true.

    temp=n
    do i=1,number_length(n)
       if (.not. is_prime(temp)) then
          is_circular_prime=.false.
          exit
       end if
       temp=rotate(temp)
    end do

  end function is_circular_prime

  pure function rotate(n)
    implicit none
    integer*8, intent(in) :: n
    integer*8             :: rotate
    integer               :: p,d

    p=number_length(n)-1
    d=mod(n,10)
    rotate=n/10 + d*(10**p)

  end function rotate

end program problem35
