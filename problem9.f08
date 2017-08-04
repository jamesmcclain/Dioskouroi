program problem9
  implicit none
  integer*8 :: a,b,c,abc

  do a=1,1000
     do b=a+1,1000
        c=1000-a-b
        if (c .lt. 1) then
           go to 1
        end if
        if (a*a + b*b == c*c) then
           abc=a*b*c
           goto 2
        end if
     end do
1    continue
  end do
2 continue

  print *, abc

end program problem9
