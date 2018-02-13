program problem65
  use fmzm
  implicit none
  integer, parameter    :: n=102,m=100
  integer               :: i
  integer, dimension(n) :: numbers
  type(im)              :: numerator,denomenator,temp

  do i=1,n,3
     numbers(i)=1
     numbers(i+1)=2*((i+2)/3)
     numbers(i+2)=1
  end do

  numerator=to_im(1)
  denomenator=to_im(numbers(m-1))
  do i=m-2,1,-1
     numerator=numbers(i)*denomenator+numerator
     temp=numerator
     numerator=denomenator
     denomenator=temp
  end do
  numerator=numerator+2*denomenator

  ! print *, im_format('i100', numerator)
  i=0
  do while (numerator>0)
     i=to_int(mod(numerator,to_im(10)))+i
     numerator=numerator/10
  end do

  print *, i

contains

end program problem65
