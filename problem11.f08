program problem11
  implicit none
  integer, parameter      :: n=20,m=4
  integer, dimension(n,n) :: numbers
  integer                 :: i,j,k,up,right,down,temp,best=-1
  
  open(33,file='./data/problem11.dat')
  read(33,*) numbers
  
  do i=1,n
     do j=1,n
        up=min(m-1,j-1)
        right=min(m-1,n-i)
        down=min(m-1,n-j)

        ! up
        best = max(best,product(numbers(i,j:j-up:-1)))

        ! up right
        temp=1
        do k=0,min(up,right)
           temp=temp*numbers(i+k,j-k)
        end do
        best = max(best,temp)

        ! right
        best = max(best,product(numbers(i:i+right,j)))

        ! down right
        temp=1
        do k=0,min(down,right)
           temp=temp*numbers(i+k,j+k)
        end do
        best = max(best,temp)
        
     end do
  end do

  print *, best
  
end program problem11
