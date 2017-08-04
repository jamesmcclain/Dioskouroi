program problem3
  use euler
  implicit none
  integer, parameter :: i16 = selected_int_kind(16)
  integer*8          :: n = 600851475143_i16
  integer            :: i

  do i=int(sqrt(dble(n)))+1,2,-1
     if (is_factor(i,n) .and. is_prime(int8(i))) then
        print *, i
        exit
     end if
  end do

end program problem3
