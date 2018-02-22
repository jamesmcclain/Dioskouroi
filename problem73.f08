program problem73
  use euler
  implicit none
  integer, parameter :: limit=12000
  real*16, parameter :: lower=1.0q0/3.0q0,upper=1.0q0/2.0q0
  real*16            :: temp
  integer            :: n,d,answer

  answer=0

  do d=2,limit
     do n=1,d
        temp=real(n,16)/real(d,16)
        if (lower<temp .and. temp<upper .and. gcd(int(n,8),int(d,8))==1) then
           answer=answer+1
        end if
     end do
  end do
  
  print *, answer

end program problem73
