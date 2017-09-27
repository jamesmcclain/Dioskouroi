program problem39
  implicit none
  integer, parameter        :: limit = 1000
  integer, dimension(limit) :: perimeters
  integer                   :: a,b,abc,best=1
  double precision          :: c

  do a=1,limit
     perimeters(a)=0
  end do

  do a=1,limit
     do b=a,limit
        c = sqrt(dble(a**2 + b**2))
        if ((a + b + c) > limit) exit
        if (mod(c,1.0) == 0.0) then
           abc=a+b+int(c)
           perimeters(abc)=perimeters(abc)+1
        end if
     end do
  end do

  do a=1,limit
     if (perimeters(a) > perimeters(best)) best=a
  end do

  print *, best

end program problem39
