program problem85
  implicit none
  integer :: besti,bestj,best,temp,i,j

  best=huge(best)
  besti=0
  bestj=0

  do i=1,100
     do j=1,100
        temp=score(i,j)
        if (abs(temp-2000000)<best) then
           besti=i
           bestj=j
           best=abs(temp-2000000)
        end if
     end do
  end do
  
  print *, best,besti*bestj

contains

  pure function score(x,y)
    implicit none
    integer, intent(in) :: x,y
    integer             :: u,v,i,j,score

    score=0
    do i=1,x
       u=(x+1-i)
       do j=1,y
          v=(y+1-j)
          score=score+(u*v)
       end do
    end do
    
  end function score
  
end program problem85
