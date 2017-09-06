program problem24
  implicit none
  integer, parameter         :: digits=10
  integer                    :: n=1000000,i,j,temp,skip
  integer, dimension(digits) :: permskip,unused,answer

  n=n-1
  do i=digits,1,-1
     unused(i)=i-1
     if (n > 0) then
        temp=factorial(i-1)
        permskip(digits+1-i)=n/temp ! sub-permutations to skip before correct order of magnitude
        n=mod(n,temp)
     else
        permskip(digits+1-i)=0
     end if
  end do

  do i=1,digits
     skip=permskip(i) ! number of unused numbers to skip
     do j=1,digits
        if (unused(j) /= -1 .and. skip == 0) then ! if no more to skip, report
           answer(i)=unused(j)
           unused(j)=-1
           goto 1
        else if (unused(j) /= -1) then ! skip
           skip=skip-1
        end if
     end do
1    continue
  end do

  print *, answer

contains

  pure function factorial(n)
    implicit none
    integer, intent(in) :: n
    integer             :: factorial,i

    factorial=1
    do i=1,n
       factorial=factorial*i
    end do
    
  end function factorial
end program problem24
