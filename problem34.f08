program problem34
  implicit none
  integer, dimension(10) :: factorials
  integer                :: i,limit,answer=0

  limit=9999999 ! seven nines > (9!)*7

  factorials(1)=1
  do i=1,9
     factorials(i+1)=factorials(i)*i
  end do

  do i=10,limit
     if (i == transmute(i)) answer=answer+i
  end do

  print *, answer

contains
  pure function transmute(n)
    implicit none
    integer, intent(in) :: n
    integer             :: transmute
    integer             :: temp

    transmute = 0
    temp=n
    do while (temp>0)
       transmute=transmute+factorials(mod(temp,10)+1)
       temp=temp/10
    end do
  end function transmute

end program problem34
