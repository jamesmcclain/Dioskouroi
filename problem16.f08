program problem16
  implicit none
  integer*16, dimension(16) :: n = (/0_16,0_16,0_16,0_16, 0_16,0_16,0_16,0_16, 0_16,0_16,0_16,0_16, 0_16,0_16,0_16,shiftl(1_16,40)/)
  integer                   :: i,s

  s=0
  
  ! Repeatedly divide the 1024-bit number by 10 to get the digits of
  ! the decimal representation.  The 1024-bit number is represented as
  ! 16 128-bit numbers (instead of 16 64-bit numbers) because extra
  ! precision is needed to handle the carries during the long
  ! division.
  do while (sum(n) > 0)
     do i=16,1,-1
        if (i > 1) then
           n(i-1)=shiftl(mod(n(i),10),64)+n(i-1) ! carry
        else
           s=s+mod(n(i),10)                      ! tally
        end if
        n(i)=n(i)/10
     end do
  end do

  print *, s
  
end program problem16
