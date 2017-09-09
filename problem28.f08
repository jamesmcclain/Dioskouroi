program problem28
  implicit none
  integer, parameter :: limit=1001
  integer            :: start,k,answer=1

  start=2
  do k=3,limit,2
     answer=answer+ring(start,k)
     start=start+k+k+(k-2)+(k-2)
  end do
  print *, answer

contains
  pure function ring(start,k)
    implicit none
    integer, intent(in) :: start,k
    integer             :: i,j
    integer             :: ring

    j=0
    ring=0
    do i=start+k+k+(k-2)+(k-2)-1,start,-1
       if (mod(j,k-1) == 0) then
          ring=ring+i
       end if
       j=j+1
    end do
  end function ring

end program problem28
