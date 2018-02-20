program problem68
  implicit none
  integer, parameter :: n=10,m=10
  integer            :: state(0:9),array(0:9)
  integer            :: i,temp
  integer*16         :: best

  best=0

  ! https://en.wikipedia.org/wiki/Heap%27s_algorithm
  do i=1,n
     state(i-1)=0
     array(i-1)=i
  end do
  call update
  
  i=0
  do while(i<n)
     if (state(i)<i) then
        if (mod(i,2)==0) then
           temp=array(0)
           array(0)=array(i)
           array(i)=temp
        else
           temp=array(state(i))
           array(state(i))=array(i)
           array(i)=temp
        end if
        call update
        state(i)=state(i)+1
        i=0
     else
        state(i)=0
        i=i+1
     end if
  end do

  print *, best

contains

  subroutine update
    implicit none
    integer, dimension(5)  :: totals
    integer, dimension(15) :: answer
    integer                :: i
    integer*16             :: temp

    if (.not. any(array(5:9)==10)) return ! 16-digit answer
    if (.not. all(array(6:9)>array(5))) return ! rotational symmetry

    totals(1)=array(5)+array(0)+array(1)
    totals(2)=array(6)+array(1)+array(2)
    totals(3)=array(7)+array(2)+array(3)
    totals(4)=array(8)+array(3)+array(4)
    totals(5)=array(9)+array(4)+array(0)

    if (all(totals==totals(1))) then ! valid answer
       answer(1)=array(5) ! first
       answer(2)=array(0)
       answer(3)=array(1)
       answer(4)=array(6) ! second
       answer(5)=array(1)
       answer(6)=array(2)
       answer(7)=array(7) ! third
       answer(8)=array(2)
       answer(9)=array(3)
       answer(10)=array(8) ! fourth
       answer(11)=array(3)
       answer(12)=array(4)
       answer(13)=array(9) ! fifth
       answer(14)=array(4)
       answer(15)=array(0)

       temp=0
       do i=1,15
          if (answer(i)>9) then
             temp=(temp*100)+answer(i)
          else
             temp=(temp*10)+answer(i)
          end if
       end do

       if (temp>best) best=temp

    end if

  end subroutine update

end program problem68
