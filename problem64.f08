program problem64
  implicit none
  integer            :: i,count
  integer, parameter :: limit=1000

  count=0
  do i=1,limit
     if (predicate(i)) count=count+1
  end do

  print *, count

contains

  pure function predicate(n)
    implicit none
    integer, intent(in) :: n
    integer             :: temp
    logical             :: predicate

    temp=period(n)
    predicate=(mod(temp,2)==1)

  end function predicate

  pure function period(n)
    implicit none
    integer, intent(in) :: n
    real*16             :: temp1,temp2
    real*16, parameter  :: cutoff=1.0q-10
    integer             :: period,i

    temp1=mod(sqrt(n*1.0q0),1.0)
    if (temp1 == 0.0) go to 100

    temp2=mod(1.0/temp1,1.0)
    do period=1,huge(period)
       if (abs(temp1-temp2)<cutoff) go to 100
       temp2=mod(1.0/temp2,1.0)
    end do

100 continue

  end function period

end program problem64
