program problem13
  implicit none
  integer, parameter             :: n=100,m=10
  integer                        :: i,digits
  double precision, dimension(n) :: numbers
  double precision               :: number
  
  open(33,file='./data/problem13.dat')
  read(33,*) numbers

  number=sum(numbers(:))
  digits=floor(log10(number))+1

  do i=1,digits-10
     number=number/10
  end do
  
  print *, int8(number)
  
end program problem13
