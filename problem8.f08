program problem8
  implicit none
  integer                  :: i,j,temp1
  integer, parameter       :: window=13,n=1000
  integer*16, dimension(n) :: numbers
  integer*16               :: temp2,best=-1

  open(33,file='./data/problem8.dat')
  read(33,*) numbers

  do i=1,n

     temp1=(i+window-1)
     if (temp1 .le. n) then
        j=temp1
     else
        j=n
     end if

     temp2=product(numbers(i:j))
     if (temp2 .gt. best) then
        best=temp2
     end if

  end do
  
  print *, best
  
end program problem8
