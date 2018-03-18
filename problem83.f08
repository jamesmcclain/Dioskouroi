program problem83
  implicit none
  integer, parameter      :: n=80
  integer, dimension(n,n) :: data
  integer, dimension(n,n) :: state
  integer                 :: i,j

  open(33,file='./data/problem83.dat')
  read(33,*) data

  do i=1,n
     do j=1,n
        state(i,j)=huge(state)
     end do
  end do

  call search(1,1,0)
  
  print *, state(n,n)

contains

  recursive subroutine search(x,y,previous)
    implicit none
    integer, intent(in) :: x,y,previous
    integer             :: current

    current=data(x,y)+previous
    if (current<state(x,y)) then
       state(x,y)=current
       if (x>1) call search(x-1,y,current)
       if (x<n) call search(x+1,y,current)
       if (y>1) call search(x,y-1,current)
       if (y<n) call search(x,y+1,current)
    end if

  end subroutine search

end program problem83
