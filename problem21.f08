program problem21
  use euler
  implicit none
  integer, parameter :: n = 10000
  integer*8          :: i, temp, sum

  sum = 0
  do i=1,n-1
     temp=sum_of_divisors(i)
     if ((i /= temp) .and. (sum_of_divisors(temp) == i)) then
        sum=sum+i
     end if
  end do

  print *, sum

end program problem21

