program problem7
  use euler
  implicit none
  integer*8 :: i=0
  integer   :: found=0,limit=10001

  do while (found < limit)
     i=i+1
     if (is_prime(i)) then
        found=found+1
     end if
  end do

  print *, i
  
end program problem7
