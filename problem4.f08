pure function digit(n,d)
  implicit none
  integer, intent(in) :: n, d
  integer             :: digit
  digit = mod(n,10**d)/(10**(d-1))
end function digit

function is_palindrome(n)
  implicit none
  integer, intent(in) :: n
  integer             :: i, j, digits, digit
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

program problem4
  implicit none
  integer :: i,j,n,best=0
  logical :: is_palindrome

  do i=999,100,-1
     do j=i,100,-1
        n = i*j
        if (is_palindrome(n) .and. (n .gt. best)) then
           best = n
        end if
     end do
  end do
  print *, best

end program problem4
