program problem2
  implicit none
  integer :: sum=0
  integer :: fn_minus_2=0,fn_minus_1=0,fn=1

  do while (fn < 4000000)
     fn_minus_2=fn_minus_1
     fn_minus_1=fn
     fn=fn_minus_2+fn_minus_1
     if (mod(fn,2)==0) then
        sum=sum+fn
     end if
  end do
  print *, sum

end program problem2

