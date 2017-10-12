program problem48
  implicit none
  integer*8 :: answer,temp,digits
  integer   :: i,j

  digits=(10_8)**(10)
  answer=0

  do i=1,1000
     temp=1
     do j=1,i
        temp=mod(temp*i,digits)
     end do
     answer=mod(answer+temp,digits)
  end do

  print *, answer

end program problem48
