program problem20
  implicit none
  integer, parameter       :: m=10 ! 640 bits should be enough
  integer*16, dimension(m) :: n
  integer                  :: i

  ! initialize number
  n(1)=1
  do i=2,m
     n(i)=0
  end do

  do i=1,100
     call multiply(n,i)
  end do

  print *, n
  print *, sum_of_digits(n)

contains
  ! not suitable for large factors
  subroutine multiply(n,f)
    integer*16, dimension(m) :: n
    integer                  :: f,i
    integer*16, parameter    :: mask=lshift(1_16,64)-1

    do i=1,m
       n(i)=n(i)*f
       if (i > 1) then
          n(i)=n(i)+rshift(n(i-1),64)
          n(i-1)=iand(n(i-1),mask)
       end if
    end do
  end subroutine multiply

  ! return sum of digits of n.  destroys n.
  function sum_of_digits(n)
    implicit none
    integer*16, dimension(m) :: n
    integer                  :: sum_of_digits

    sum_of_digits=0
    do while (sum(n) > 0)
       do i=m,1,-1
          if (i > 1) then
             n(i-1)=shiftl(mod(n(i),10),64)+n(i-1)    ! carry
          else
             sum_of_digits=sum_of_digits+mod(n(i),10) ! tally
          end if
          n(i)=n(i)/10
       end do
    end do
  end function sum_of_digits

end program problem20
