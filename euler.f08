module euler  
  implicit none 
  private
  public :: binomial
  public :: digit
  public :: gcd
  public :: is_factor
  public :: is_palindrome
  public :: is_prime
  public :: number_length
  public :: sum_of_divisors
  public :: tau
contains      

  ! Binomial coefficient
  pure function binomial(n,k)
    implicit none
    integer*8, intent(in) :: n,k
    integer*16            :: temp
    integer*8             :: i,binomial

    temp=1
    do i=n-k+1,n
       temp=temp*i
    end do
    do i=1,n-k
       temp=temp/i
    end do
    binomial=temp
  end function binomial
  
  ! Get the dth digit from the decimal representation of n.
  pure function digit(n,d)
    implicit none
    integer, intent(in) :: n, d
    integer             :: digit
    digit = mod(n,10**d)/(10**(d-1))
  end function digit

  ! gcd
  pure function gcd(a0,b0)
    implicit none
    integer*8, intent(in) :: a0, b0
    integer               :: a, b, t, gcd

    a=a0
    b=b0
    do while (b /= 0)
       t=b
       b=mod(a,b)
       a=t
    end do
    gcd=a
  end function gcd

  ! Returns .true. if f is a factor of n, otherwise returns .false.
  pure function is_factor(f,n)
    implicit none
    integer*8, intent(in) :: n
    integer, intent(in)   :: f
    logical               :: is_factor
    is_factor = (mod(n,f)==0)
  end function is_factor

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

  ! Returns .true. if n is a prime number, otherwise return .false.
  pure function is_prime(n)
    implicit none
    integer*8, intent(in) :: n
    integer               :: i
    logical               :: is_prime

    if (n == 1) then
       is_prime = .false.
    else
       is_prime = .true.
       do i=2,min(n-1,ceiling(sqrt(dble(n))))
          if (mod(n,i) == 0) then
             is_prime = .false.
             exit
          end if
       end do
    end if
  end function is_prime

  ! Get the length of the decimal representation of a number
  pure function number_length(n)
    implicit none
    integer*8, intent(in) :: n
    integer               :: number_length
    number_length = ceiling(log10(float(n)))
  end function number_length

  ! Returns the sum of proper divisors
  pure function sum_of_divisors(n)
    implicit none
    integer*8, intent(in) :: n
    integer*8             :: i
    integer*8             :: sum_of_divisors

    sum_of_divisors=1
    do i=2,n-1
       if (mod(n,i) == 0) then
          sum_of_divisors=sum_of_divisors+i
       end if
    end do
  end function sum_of_divisors

  ! Euler tau function: The number of divisors of n
  pure function tau(n)
    implicit none
    integer*8, intent(in) :: n
    integer*8             :: current_n,p
    integer               :: a,tau

    tau=1
    current_n=n

1   if (is_prime(current_n)) then ! shortcut for prime numbers
       tau=tau*2
       current_n=1
    end if
    do p=2,current_n
       if (is_prime(p) .and. (mod(current_n,p) == 0)) then
          do a=ceiling(log(dble(n))/log(dble(p))),1,-1
             if (mod(current_n,p**a) == 0) then
                exit
             end if
          end do
          tau=tau*(a+1)
          current_n=current_n/(p**a)
          go to 1
       end if
    end do
  end function tau

end module euler
