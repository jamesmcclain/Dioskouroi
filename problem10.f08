program problem10
  use euler
  implicit none
  integer, parameter :: limit=2000000
  integer            :: i
  integer*16         :: sum=2

  do i=3,limit-1,2
     if (is_prime(int8(i))) then
        sum=sum+i
     end if
  end do
  
  print *, sum

end program problem10
