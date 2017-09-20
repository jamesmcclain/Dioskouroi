program problem36
  use euler
  implicit none
  integer, parameter :: limit=1000000
  integer            :: i,answer=0

  do i=1,limit-1
     if (is_palindrome(i) .and. is_binary_palindrome(i)) then
        answer=answer+i
     end if
  end do
  
  print *, answer

contains
  function is_binary_palindrome(n)
    implicit none
    integer, intent(in)      :: n
    integer                  :: temp,digits,i
    integer*1, dimension(32) :: bits
    logical                  :: is_binary_palindrome
    real*8                   :: temp2

    temp2=log(dble(n))/log(dble(2))
    if (mod(temp2,1.0_8) == 0.0_8) temp2=temp2+0.1
    digits = ceiling(temp2)
    is_binary_palindrome=.true.
    temp=n

    do i=1,digits
       bits(i)=mod(temp,2)
       temp=temp/2
    end do

    do i=1,digits
       if (bits(i) /= bits(digits+1-i)) then
          is_binary_palindrome=.false.
          exit
       end if
    end do
  end function is_binary_palindrome

end program problem36
