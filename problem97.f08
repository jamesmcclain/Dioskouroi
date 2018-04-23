program problem97
  implicit none
  integer            :: i
  integer*16         :: number,modulus
  integer, parameter :: power=7830457

  modulus=1_16
  do i=1,10
     modulus=modulus*10
  end do
  
  number=28433_16
  do i=1,power
     number=number*2
     number=mod(number,modulus)
  end do
  number=number+1

  print *,number

end program problem97
