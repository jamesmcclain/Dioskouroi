program problem27
  use euler
  implicit none
  integer   :: a=1,b=41,n=39,i,j,k
  integer*8 :: temp

  do i=-999,999
     do j=-1000,1000

        ! Quick filter
        temp=n*n + i*n + j
        if (.not. is_prime(abs(temp))) then
           cycle
        end if

        ! Compute number of consecutive primes
        do k=0,1000*1000*1000
           temp=k*k + i*k + j
           if (.not. is_prime(abs(temp))) then
              exit
           end if
        end do

        ! Update if better
        if (k>n) then
           a=i
           b=j
           n=k
        end if
     end do
  end do

  print *, a*b

end program problem27
