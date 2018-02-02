program problem62
  implicit none
  integer, parameter :: n=10000,p=10000019
  integer            :: i
  integer*8          :: counts(0:p-1),temp

  do i=0,p-1
     counts(i)=0
  end do

  do i=1,n
     temp=hash(i)
     counts(temp)=counts(temp)+1
  end do

  do i=1,n
     temp=hash(i)
     if (counts(temp) == 5) then
        go to 100
     end if
  end do

100 continue
  print *, (i*1_16)**3

contains

  function hash(n)
    implicit none
    integer             :: primes(0:9)
    integer, intent(in) :: n
    integer*16          :: temp
    integer*8           :: hash

    primes=(/ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 /)
    hash=1
    temp=(n*1_16)**3

    do while (temp>0)
       hash=hash*primes(mod(temp,10))
       temp=temp/10
    end do

    hash=mod(hash,p)

  end function hash

end program problem62
