program problem99
  implicit none
  integer, parameter      :: n=1000
  integer, dimension(2,n) :: pairs
  integer                 :: i
  real*16, dimension(n)   :: logs

  open(33,file='./data/problem99.dat')
  read(33,*) pairs

  do i=1,n
     logs(i)=real(pairs(2,i),16)*log(real(pairs(1,i),16))
  end do
  
  print *, maxloc(logs)

end program problem99
