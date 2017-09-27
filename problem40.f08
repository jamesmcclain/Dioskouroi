program problem40
  use euler
  implicit none
  integer, parameter    :: limit=10**6
  integer               :: n,i,j,digits,target=1,answer=1

  n=1 ! the current integer
  i=1 ! the current digit

  do while (i<=limit)
     do j=number_length(int8(n)),1,-1
        if (i == target) then
           answer=answer*digit(n,j)
           target=target*10
        end if
        i=i+1
        if (i>limit) exit
     end do
     n=n+1
  end do

  print *, answer

end program problem40
