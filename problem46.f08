program problem46
  use euler
  implicit none
  integer*8 :: answer

  do answer=33,huge(answer),2
     if (is_prime(answer)) cycle
     if (.not. goldbach(answer)) exit
  end do

  print *, answer

contains

  pure function goldbach(c)
    integer*8, intent(in) :: c
    integer*8             :: i,p
    logical               :: goldbach

    goldbach=.false.
    do i=1,c
       p=c-2*i*i
       if (p<=0) exit
       if (is_prime(p)) then
          goldbach=.true.
          exit
       end if
    end do

  end function goldbach

end program problem46
