program problem18
  implicit none
  integer, parameter    :: m=15,n=120
  integer, dimension(n) :: numbers,scratch
  integer               :: i,j,temp,index1,index2,level_start

  open(33,file='./data/problem18.dat')
  read(33,*) numbers

  do i=1,n
     scratch(i)=-1
  end do
  scratch(1)=numbers(1)

  level_start=0
  do i=1,m-1
     do j=1,i
        index1=level_start+j
        index2=index1+i
        temp=scratch(index1)+numbers(index2)
        if (temp>scratch(index2)) then
           scratch(index2)=temp
        end if

        index2=index1+i+1
        temp=scratch(index1)+numbers(index2)
        if (temp>scratch(index2)) then
           scratch(index2)=temp
        end if
     end do
     level_start=level_start+i
  end do

  print *, maxval(scratch(level_start+1:n))

end program problem18

