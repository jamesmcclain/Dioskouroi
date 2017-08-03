program problem5
  use euler
  implicit none
  integer*8 :: i,n=1
  integer   :: limit=20

  do i=2,limit
     n=n*(i/gcd(i,n))
  end do
  print *, n

end program problem5
