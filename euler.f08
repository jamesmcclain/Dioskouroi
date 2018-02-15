module euler  
  implicit none 
  private
  public :: binomial
  public :: digit
  public :: digit16
  public :: gcd
  public :: is_factor
  public :: is_palindrome
  public :: is_palindrome16
  public :: is_prime
  public :: number_length
  public :: sum_of_divisors
  public :: tau
  public :: is_triangular
  public :: is_square
  public :: is_pentagonal
  public :: is_hexagonal
  public :: is_heptagonal
  public :: is_octagonal

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

  ! Get the dth digit from the decimal representation of n.
  pure function digit16(n,d)
    implicit none
    integer*16, intent(in) :: n
    integer, intent(in)    :: d
    integer                :: digit16
    digit16 = mod(n,10_16**d)/(10_16**(d-1))
  end function digit16

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

  ! Returns .true. if the decimal representation of n is a palindrome,
  ! otherwise returns .false.
  pure function is_palindrome16(n)
    implicit none
    integer*16, intent(in) :: n
    real*16                :: temp
    integer                :: i, j, digits
    logical                :: is_palindrome16

    temp=n*1.0
    digits = ceiling(log10(temp))
    is_palindrome16 = .true.

    do i=1,digits
       j = digits+1-i
       if (digit16(n,i) /= digit16(n,j)) then
          is_palindrome16 = .false.
          exit
       end if
    end do
  end function is_palindrome16

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
    real*8                :: temp

    temp=log10(dble(n))
    if (mod(temp,1.0_8)==0.0_8) temp=temp+0.01
    number_length = ceiling(temp)
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

  pure function is_triangular(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_triangular,plus,minus

    temp=(1 + sqrt(1.0 + 4*2*m))/2
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(1 - sqrt(1.0 + 4*2*m))/2
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_triangular=plus .or. minus

  end function is_triangular

  pure function is_square(m)
    implicit none
    integer, intent(in) :: m
    logical             :: is_square

    is_square=mod(sqrt(dble(m)),1.0)==0.0

  end function is_square

  pure function is_pentagonal(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_pentagonal,plus,minus

    temp=(1 + sqrt(1.0 + 4*3*2*m))/6
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(1 - sqrt(1.0 + 4*3*2*m))/6
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_pentagonal=plus .or. minus

  end function is_pentagonal

  pure function is_hexagonal(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_hexagonal,plus,minus

    temp=(1 + sqrt(1.0 + 4*2*m))/4
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(1 - sqrt(1.0 + 4*2*m))/4
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_hexagonal=plus .or. minus

  end function is_hexagonal

  pure function is_heptagonal(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_heptagonal,plus,minus

    temp=(3 + sqrt(9.0 + 4*5*2*m))/10
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(3 - sqrt(9.0 + 4*5*2*m))/10
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_heptagonal=plus .or. minus

  end function is_heptagonal

  pure function is_octagonal(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_octagonal,plus,minus

    temp=(2 + sqrt(4.0 + 4*3*m))/6
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(2 - sqrt(4.0 + 4*3*m))/6
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_octagonal=plus .or. minus

  end function is_octagonal

end module euler
