program problem12
  use euler
  implicit none
  integer*8          :: i,triangle=1
  integer, parameter :: n=500
  
  i=2
  do while (.true.)
     triangle=triangle+i
     i=i+1
     if (tau(triangle) .gt. n) then
        exit
     end if
  end do

  print *, triangle
  
end program problem12
