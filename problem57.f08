program problem57
  use fmzm
  implicit none
  type(im),save      :: pm,pn,qm,qn,temp
  real*8             :: pdigits, qdigits
  integer            :: i,count
  integer, parameter :: limit=1000

  pm = 0
  pn = 1
  qm = 2
  qn = 2

  count=0
  
  ! References:
  ! http://mathworld.wolfram.com/PellNumber.html
  ! https://en.wikipedia.org/wiki/Pell_number
  ! https://en.wikipedia.org/wiki/Square_root_of_2
  do i=1,limit
     temp=pn
     pn=2*pn+pm
     pm=temp

     temp=qn
     qn=2*qn+qm
     qm=temp

     qdigits=ceiling(to_dp(log10(to_fm(qn) / to_fm('2.0'))))
     pdigits=ceiling(to_dp(log10(to_fm(pn))))

     if (qdigits > pdigits) count=count+1
  end do

  print *, count

end program problem57
