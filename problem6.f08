program problem6
  implicit none
  integer*8 :: diff=0
  integer   :: i,j,limit=100

  do i=1,limit
     do j=1,limit
        if (i /= j) then
           diff = diff + i*j
        end if
     end do
  end do
  
  print *, diff

end program problem6
