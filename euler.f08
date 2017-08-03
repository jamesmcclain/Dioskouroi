module euler  
  implicit none 
  private
  public :: digit, is_palindrome
  
contains      
  ! Get the dth digit from the decimal representation in n.
  pure function digit(n,d)
    implicit none
    integer, intent(in) :: n, d
    integer             :: digit
    digit = mod(n,10**d)/(10**(d-1))
  end function digit

  ! Return .true. if the decimal representation of n is a palindrome,
  ! return .false. otherwise.
  pure function is_palindrome(n)
    implicit none
    integer, intent(in) :: n
    integer             :: i, j, digits
    logical             :: is_palindrome

    digits = ceiling(log10(dble(n)))
    is_palindrome = .true.

    do i=1,digits
       j = digits+1-i
       if (digit(n,i) /= digit(n,j)) then
          is_palindrome = .false.
          exit
       end if
    end do

  end function is_palindrome

end module euler
