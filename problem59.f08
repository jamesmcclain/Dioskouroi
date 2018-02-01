program problem59
  implicit none
  integer, parameter      :: n=1201
  integer*1, dimension(n) :: numbers
  integer                 :: frequency(0:255)
  integer*1, dimension(3) :: key
  integer                 :: i,j,temp,count

  open(33,file='./data/problem59.dat')
  read(33,*) numbers
  close(33)

  do j=1,3
     count=0

     do i=0,255
        frequency(i)=0
     end do

     do i=j,n,3
        temp=frequency(numbers(i))+1
        frequency(numbers(i))=temp
        if (temp>count) then
           count=temp
           key(j)=numbers(i)
        end if
     end do
     key(j)=xor(key(j),ichar(' '))
  end do

  count=0
  do i=1,n
     temp=xor(numbers(i),key(mod(i-1,3)+1))
     count=count+temp
  end do

  print *, count

end program problem59
