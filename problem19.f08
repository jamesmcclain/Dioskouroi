program problem19
  implicit none
  integer :: year,month,day,count=0

  ! January 1, 1901 was a Tuesday
  day=2

  do year=1901,2000
     do month=1,12
        if (mod(day,7)==0) then
           count=count+1
        end if
        select case (month)
        case (9,4,6,11)
           day=day+30
        case (2)
           if (mod(year,4)==0 .and. (mod(year,100)/=0 .or. mod(year,400)==0)) then
              day=day+29
           else
              day=day+28
           end if
        case default
           day=day+31
        end select
     end do
  end do

  print *, count

end program problem19
