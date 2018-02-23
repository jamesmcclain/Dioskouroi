program problem74
  implicit none
  integer, parameter :: factorials(0:9) = (/ 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880 /)
  integer            :: i,answer

  answer=0
  do i=1,1000000
     if (predicate(i)) answer=answer+1
  end do
  
  print *, answer

contains

  pure function step(n)
    implicit none
    integer*16, intent(in) :: n
    integer*16             :: temp,step

    temp=n
    step=0
    do while(temp>0)
       step=step+factorials(mod(temp,10))
       temp=temp/10
    end do

  end function step

  pure function predicate(n)
    implicit none
    integer, intent(in)      :: n
    integer                  :: i
    integer, parameter       :: m=60
    integer*16, dimension(m) :: trajectory
    logical                  :: predicate

    trajectory(1)=int(n,16)
    do i=2,m
       trajectory(i)=step(trajectory(i-1))
    end do

    predicate=all(trajectory(1:m-1) /= trajectory(m))

  end function predicate

end program problem74
