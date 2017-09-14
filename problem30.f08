program problem30
  implicit none
  integer, parameter :: limit=999999
  integer            :: i
  integer            :: answer=0

  do i=2,limit
     if (predicate(i)) then
        answer=answer+i
     end if
  end do

  print *, answer

contains
  pure function predicate(n)
    implicit none
    integer, intent(in) :: n
    integer             :: current,sum
    logical             :: predicate

    current=n
    sum=0
    do while (current > 0)
       sum=sum+(mod(current,10)**5)
       current=current/10
    end do
    predicate=(sum == n)

  end function predicate

end program problem30
