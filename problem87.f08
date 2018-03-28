program problem87
  implicit none
  integer, parameter        :: m=664579,limit=50000000
  real*8, parameter         :: alimit=sqrt(dble(limit)),blimit=dble(limit)**(1.0/3.0),climit=dble(limit)**(1.0/4.0)
  integer                   :: a,b,c,temp
  integer, dimension(limit) :: found
  integer, dimension(m)     :: primes

  ! Read primes
  open(33,file='./data/problem70.dat')
  read(33,*) primes

  ! Initialize
  do a=1,limit
     found(a)=0
  end do

  print *, alimit,blimit,climit

  do a=1,huge(1)
     if (primes(a)>alimit) exit
     do b=1,huge(1)
        if (primes(b)>blimit) exit
        do c=1,huge(1)
           if (primes(c)>climit) exit
           temp=primes(a)**2 + primes(b)**3 + primes(c)**4
           if (temp<=limit) then
              found(temp)=1
           end if
        end do
     end do
  end do
  
  print *, sum(found)

contains

end program problem87
