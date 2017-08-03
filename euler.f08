module euler  
  implicit none 
  private
  public :: digit, is_palindrome, is_factor, is_prime

contains      
  ! Get the dth digit from the decimal representation of n.
  pure function digit(n,d)
    implicit none
    integer, intent(in) :: n, d
    integer             :: digit
    digit = mod(n,10**d)/(10**(d-1))
  end function digit

  ! Returns .true. if the decimal representation of n is a palindrome,
  ! otherwise returns .false.
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

  ! Returns .true. if f is a factor of n, otherwise returns .false.
  pure function is_factor(f,n)
    implicit none
    integer*8, intent(in) :: n
    integer, intent(in)   :: f
    logical               :: is_factor
    is_factor = (mod(n,f)==0)
  end function is_factor

  ! Returns .true. if n is a prime number, otherwise return .false.
  pure function is_prime(n)
    implicit none
    integer, intent(in) :: n
    integer             :: i
    logical             :: is_prime

    is_prime = .true.
    do i=2,int(sqrt(real(n)))+1
       if (mod(n,i) == 0) then
          is_prime = .false.
          exit
       end if
    end do
  end function is_prime

end module euler
