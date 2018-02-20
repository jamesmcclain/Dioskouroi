program problem71
  use euler
  implicit none
  real*16, parameter :: target=(3.0q0/7.0q0)
  real*16            :: tempr,best
  integer, parameter :: limit=1000000
  integer*8          :: tempi,num,denom

  best=-huge(best)

  do denom=1,limit
     tempi=int8(floor(target*denom))
     tempr=real(tempi,16)/denom
     if (best<tempr .and. tempr<target) then
        best=tempr
        num=tempi/(gcd(tempi,denom))
     end if
  end do

  print *, best, num

end program problem71
