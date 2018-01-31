program problem60
  use euler
  implicit none
  integer, parameter          :: limit=10000
  logical, dimension(limit)   :: primesl
  integer*8, dimension(limit) :: primes
  integer*8                   :: i,a,b,c,d,e
  integer                     :: count
  logical, dimension(4)       :: ps

  count=0
  do i=1,limit
     if (is_prime(i) .and. (mod(i,3)/=2)) then
        count=count+1
        primes(count)=i
        primesl(i)=.true.
     else
        primesl(i)=.false.
     end if
  end do

  do a=1,count
     do b=a+1,count
        ps(1)=predicate(primes(a),primes(b))
        if (.not. ps(1)) cycle
        do c=b+1,count
           ps(1)=predicate(primes(a),primes(c))
           ps(2)=predicate(primes(b),primes(c))
           if (.not. (ps(1) .and. ps(2))) cycle
           do d=c+1,count
              ps(1)=predicate(primes(a),primes(d))
              ps(2)=predicate(primes(b),primes(d))
              ps(3)=predicate(primes(c),primes(d))
              if (.not. (ps(1) .and. ps(2) .and. ps(3))) cycle
              do e=d+1,count
                 ps(1)=predicate(primes(a),primes(e))
                 ps(2)=predicate(primes(b),primes(e))
                 ps(3)=predicate(primes(c),primes(e))
                 ps(4)=predicate(primes(d),primes(e))
                 if (.not. (ps(1) .and. ps(2) .and. ps(3) .and. ps(4))) cycle
                 go to 100
              end do
           end do
        end do
     end do
  end do

100 continue
  print *, primes(a)+primes(b)+primes(c)+primes(d)+primes(e)

contains

  pure function predicate(p1,p2)
    implicit none
    integer*8, intent(in) :: p1,p2
    integer*8             :: temp1,temp2,len1,len2
    logical               :: predicate

    len1=int8(ceiling(log10(dble(p1))))
    len2=int8(ceiling(log10(dble(p2))))
    temp1=p1+p2*(10**(len1))
    temp2=p2+p1*(10**(len2))

    predicate = is_prime(temp1) .and. is_prime(temp2)
    
  end function predicate
  
end program problem60
