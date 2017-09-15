program problem31
  implicit none
  integer, parameter :: limit=200
  integer, parameter :: av=200,bv=100,cv=50,dv=20,ev=10,fv=5,gv=2,hv=1
  integer            :: a,b,c,d,e,f,g,h,answer=0

  do a=0,(limit/av)
     do b=0,(limit/bv)
        if (a*av + b*bv > limit) exit
        do c=0,(limit/cv)
           if (a*av + b*bv + c*cv > limit) exit
           do d=0,(limit/dv)
              if (a*av + b*bv + c*cv + d*dv > limit) exit
              do e=0,(limit/ev)
                 if (a*av + b*bv + c*cv + d*dv + e*ev > limit) exit
                 do f=0,(limit/fv)
                    if (a*av + b*bv + c*cv + d*dv + e*ev + f*fv > limit) exit
                    do g=0,(limit/gv)
                       if (a*av + b*bv + c*cv + d*dv + e*ev + f*fv + g*gv > limit) exit
                       h = limit - (a*av + b*bv + c*cv + d*dv + e*ev + f*fv + g*gv)
                       if (0 <= h .and. h <= limit) answer=answer+1
                    end do
                 end do
              end do
           end do
        end do
     end do
  end do

  print *, answer

end program problem31
