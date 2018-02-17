program problem70
  implicit none
  integer, parameter    :: m=664579,limit=10000000
  integer, dimension(m) :: primes
  integer               :: n,phi_n,answer
  real*8                :: n_over_phi_n,best

  open(33,file='./data/problem70.dat')
  read(33,*) primes

  best=huge(best)
  
  do n=2,limit
     phi_n=phi(n)
     if (predicate(n,phi_n)) then
        n_over_phi_n=real(n)/phi_n
        if (n_over_phi_n<best) then
           best=n_over_phi_n
           answer=n
        end if
     end if
  end do

  print *, answer, phi(answer), best

contains

  pure function predicate(x,y)
    implicit none
    integer, intent(in) :: x,y
    integer             :: a,b
    integer*16          :: temp1,temp2
    logical             :: predicate

    a=x
    b=y
    temp1=1
    temp2=1
    do while (a>0 .or. b>0)
       temp1=temp1*primes(mod(a,10)+1)
       temp2=temp2*primes(mod(b,10)+1)
       a=a/10
       b=b/10
    end do

    predicate=(temp1==temp2)

  end function predicate

  function phi(n)
    implicit none
    integer, intent(in) :: n
    integer             :: p,phi,i,rest
    logical             :: factor_found

    factor_found=.false.
    phi=n
    rest=n
    do i=1,n
       p=primes(i)
       if (mod(rest,p)==0) then
          phi=(phi/p)*(p-1)
          factor_found=.true.
          do while (mod(rest,p)==0)
             rest=rest/p
          end do
       end if

       ! Important: if the current prime is more than the square root
       ! of what is left, then what is left must be the last prime
       ! factor.
       if (p>sqrt(real(rest))) then
          if (rest>1) phi=(phi/rest)*(rest-1)
          exit
       end if
    end do

  end function phi

end program problem70
