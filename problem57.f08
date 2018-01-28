program problem57
  implicit none
  integer*16           :: pm,pn,qm,qn,temp
  integer              :: i
  integer, parameter   :: limit=1000

  pm = 0
  pn = 1
  qm = 2
  qn = 2

  ! Reference: http://mathworld.wolfram.com/PellNumber.html
  do i=1,limit
     print *, i, qn, pn

     temp=pn
     pn=2*pn+pm
     pm=temp

     temp=qn
     qn=2*qn+qm
     qm=temp
  end do

end program problem57
