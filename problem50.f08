program problem50
  use euler
  implicit none
  integer, parameter             :: limit=1000000
  integer*8, dimension(limit/10) :: primes,sums
  integer*8                      :: i,j,sum,count
  integer*8                      :: best_prime,best_gap,gap,prime

  count=0
  do i=2,limit
     if (is_prime(int8(i))) then
        count=count+1
        primes(count)=i
     end if
  end do

  sum=0
  do i=1,count
     sum=sum+primes(i)
     sums(i)=sum
  end do

  best_gap=21
  do i=count,1,-1 ! index of end of sequence
     do j=1,i-1   ! length of the gap
        gap=i-j+1
        if (gap<best_gap) exit
        prime=sums(i)-sums(j)
        if (prime<limit .and. is_prime(prime)) then
           best_prime=sums(i)-sums(j)
           best_gap=i-j+1
           print *, i,j,sums(i),sums(j),best_prime, best_gap
        end if
     end do
  end do
  
print *, best_prime, best_gap

contains

end program problem50
