program problem55
  use euler
  implicit none
  integer*16         :: i
  integer            :: count
  integer, parameter :: limit=10000

  count=0
  do i=1,limit
     if (lychrel(i)) count=count+1
  end do
  
  print *, count

contains

  pure function step(n)
    implicit none
    integer*16, intent(in) :: n
    integer                :: i, digits
    real*16                :: temp
    integer*16             :: step

    temp=n*1.0
    step=n
    digits=int(ceiling(log10(temp)))
    do i=1,digits
       step=step+mod(n/10_16**(digits-i),10)*(10_16**(i-1))
    end do

  end function step

  pure function lychrel(n)
    implicit none
    integer*16, intent(in) :: n
    integer*16             :: temp
    integer                :: i
    logical                :: lychrel

    lychrel=.true.
    temp=n
    do i=1,50
       temp=step(temp)
       if (is_palindrome16(temp)) then
          lychrel=.false.
          exit
       end if
    end do
    
  end function lychrel
  
end program problem55
