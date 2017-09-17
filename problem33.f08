program problem33
  use euler
  implicit none
  integer   :: a1,a2,b1,b2
  integer*8 :: a=1,b=1
  real      :: temp1,temp2

  do b1=0,9
     do b2=0,9
        if (b1==0 .and. b2==0) cycle ! undefined
        do a2=0,9
           if (a2==0 .and. b2==0) cycle ! "trivial"
           do a1=0,9
              if (a1==0 .and. b1==0) cycle ! "trivial"
              temp1=float(a1*10+a2)/(b1*10+b2)
              if (temp1 >= 1.0 .or. temp1 <= 0) cycle ! must be positive fraction less than one
              if (a1==b1 .and. (float(a2)/b2)==temp1) then
                 a=a*a2
                 b=b*b2
              end if
              if (a2==b2 .and. (float(a1)/b1)==temp1) then
                 a=a*a1
                 b=b*b1
              end if
              if (a2==b1 .and. (float(a1)/b2)==temp1) then
                 a=a*a1
                 b=b*b2
              end if
              if (a1==b2 .and. (float(a2)/b1)==temp1) then
                 a=a*a2
                 b=b*b1
              end if
           end do
        end do
     end do
  end do

  print *, b/(gcd(a,b))

end program problem33
