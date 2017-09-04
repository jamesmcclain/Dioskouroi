program problem22
  implicit none
  integer, parameter         :: n=5163,m=20
  character(m), dimension(n) :: names
  character(m)               :: temp
  integer                    :: i,j,answer

  open(33,file='./data/problem22.dat')
  read(33,*) names

  ! sort
  do i=1,n
     do j=i,n
        if (names(i) > names(j)) then
           temp=names(i)
           names(i)=names(j)
           names(j)=temp
        end if
     end do
  end do

  answer=0
  do i=1,n
     answer=answer+(i*score(names(i)))
  end do

  print *, answer

contains
  ! score string
  pure function score(name)
    implicit none
    character(m), intent(in) :: name
    integer                  :: score,i

    score=0
    do i=1,len_trim(name)
       score=score+ichar(name(i:i))-ichar('@')
    end do

  end function score

end program problem22
