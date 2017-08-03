program problem4
  use euler
  implicit none
  integer :: i,j,n,best=0

  do i=999,100,-1
     do j=i,100,-1
        n = i*j
        if (is_palindrome(n) .and. (n .gt. best)) then
           best = n
        end if
     end do
  end do
  print *, best

end program problem4
