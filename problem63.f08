program problem63
  implicit none
  integer    :: i,j,temp,number

  number=0
  do i=1,10
     do j=1,200
        temp=count_digits((1q0*i)**j)
        if (temp==j) number=number+1
     end do
  end do

  print *, number

contains

  pure function count_digits(n)
    real*16, intent(in) :: n
    real*16             :: temp
    integer             :: count_digits

    temp=log10(n)
    count_digits=int(ceiling(temp))
    if (temp==0.0) then
       count_digits=1
    else if (mod(temp,1.0q0)==0.0q0) then
       count_digits=count_digits-1
    end if

  end function count_digits

end program problem63
