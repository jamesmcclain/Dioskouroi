program problem92
  implicit none
  integer, parameter           :: mlimit=10000000,tlimit=1000000
  integer*1, dimension(mlimit) :: memo
  integer                      :: i,answer

  do i=1,mlimit
     memo(i)=0
  end do
  memo(1)=1
  memo(89)=89

  do i=1,mlimit
     call trajectory(i)
  end do

  answer=0
  do i=1,mlimit-1
     if(memo(i)==89) answer=answer+1
  end do

  print *, answer

contains

  subroutine trajectory(n)
    implicit none
    integer, intent(in)        :: n
    integer, dimension(tlimit) :: history
    integer                    :: i,current

    current=n
    i=0
    do while(memo(current)/=1 .and. memo(current)/=89) ! follow trajectory
       i=i+1
       history(i)=current
       current=step(current)
    end do
    do i=i,1,-1 ! record results in memo
       memo(history(i))=memo(current)
    end do

  end subroutine trajectory

  pure function step(n)
    implicit none
    integer, intent(in) :: n
    integer             :: step,remaining

    step=0
    remaining=n
    do while (remaining>0)
       step=step+mod(remaining,10)**2
       remaining=remaining/10
    end do
    
  end function step

end program problem92
