program problem23
  use euler
  implicit none
  integer, parameter    :: n=28123
  integer               :: i,j,temp,count=0
  integer, dimension(n) :: abundants
  logical, dimension(n) :: interesting
  integer*8             :: answer=0

  do i=1,n
     interesting(i)=.true.
     if (sum_of_divisors(int8(i)) > i) then
        count=count+1
        abundants(count)=i
     end if
  end do

  do i=1,count
     do j=1,count
        temp=abundants(i)+abundants(j)
        if (temp <= n) then
           interesting(temp)=.false.
        end if
     end do
  end do

  do i=1,n
     if (interesting(i)) then
        answer=answer+i
     end if
  end do

  print *, answer

end program problem23

