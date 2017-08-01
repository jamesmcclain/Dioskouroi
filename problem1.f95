program problem1
  implicit none
  integer :: i,sum

  do i=0,(1000-1)
     if (mod(i,3) == 0 .or. mod(i,5) == 0) then
        sum = sum + i
     end if
  end do
  print *, sum

end program problem1

