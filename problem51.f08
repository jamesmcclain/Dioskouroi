program problem51
  use euler
  implicit none
  integer*8 :: p,hits,temp
  integer   :: bitmask,tempdigits
  integer*1 :: d
  logical   :: enable
  integer, parameter :: limit=8

  do p=2,huge(p)
     if (.not. is_prime(p)) cycle
     bitmask=1
     do while(workable(p,bitmask)) ! bitmask "<=" than prime
        hits=0
        enable=.false.
        do d=0,9
           temp=replace(p,d,bitmask)
           if (is_prime(temp) .and. workable(temp,bitmask)) then ! prime, "<=" bitmask
              if (p==temp) enable=.true.                         ! make sure prime is part of family
              hits=hits+1                                        ! bitmask "<=" than temp
           end if
           if (hits>=limit .and. enable) go to 100
        end do
        bitmask=bitmask+1
     end do
  end do

100 continue
  print *, p
  
contains

  pure function workable(number,bitmask)
    implicit none
    integer*8, intent(in) :: number
    integer, intent(in)   :: bitmask
    integer*8             :: temp1
    integer               :: temp2
    logical               :: workable

    temp1=number
    temp2=bitmask
    workable=.true.
    do while(temp1>0)
       temp1=temp1/10
       temp2=temp2/2
    end do

    if (temp2>0) then
       workable=.false.
    end if

  end function workable

  pure function replace(number,digit,bitmask)
    implicit none
    integer*8, intent(in) :: number
    integer*1, intent(in) :: digit
    integer, intent(in)   :: bitmask
    integer               :: temp1,i
    integer*8             :: temp2,replace

    temp1=bitmask
    temp2=number
    replace=number
    i=0
    do while (temp2>0)
       if (mod(temp1,2) == 1) then ! if bit asserted
          replace=replace-(mod(temp2,10)*(10**i))
          replace=replace+(digit*(10**i))
       end if
       temp1=temp1/2
       temp2=temp2/10
       i=i+1
    end do

  end function replace

end program problem51
