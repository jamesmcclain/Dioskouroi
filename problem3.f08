pure function is_factor(f,n)
  integer*8, intent(in) :: n
  integer, intent(in)   :: f
  logical               :: is_factor
  is_factor = (mod(n,f)==0)
end function is_factor

pure function is_prime(n)
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

program problem3
  implicit none
  integer, parameter :: i16 = selected_int_kind(16)
  integer*8          :: n = 600851475143_i16
  integer            :: i
  logical            :: is_factor, is_prime

  do i=2,int(sqrt(dble(n)))
     if (is_factor(i,n) .and. is_prime(i)) then
        print *, i
     end if
  end do

end program problem3
