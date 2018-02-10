program problem64
  use fmzm
  implicit none
  integer            :: i,count
  integer, parameter :: limit=10000

  call fm_set(1000)

  count=0
  do i=1,limit
     if (predicate(i)) count=count+1
  end do

  print *, count

contains

  function predicate(n)
    implicit none
    integer, intent(in) :: n
    integer             :: temp
    logical             :: predicate

    temp=period(n)
    predicate=(mod(temp,2)==1)

  end function predicate

  function period(n)
    use fmzm
    implicit none
    integer, intent(in) :: n
    type(fm)            :: temp1,temp2
    type(fm)            :: cutoff,one
    integer             :: period,i

    cutoff=to_fm(1.0d-10)
    one=to_fm(1.0)

    temp1=mod(sqrt(to_fm(n)),one)
    if (temp1 == 0.0) go to 100

    temp2=mod(one/temp1,one)
    do period=1,huge(period)
       if (abs(temp1-temp2)<cutoff) go to 100
       temp2=mod(one/temp2,one)
    end do

100 continue

  end function period

end program problem64
