program problem25
  implicit none
  integer, parameter       :: m=52 ! 64*52=3328 bits is sufficient
  integer*16, parameter    :: magic=27421685952577297_16 ! 10^999 / ((2^64)^51)
  integer*16, dimension(m) :: f1,f2
  integer                  :: i

  f1(1)=1
  f2(1)=1
  do i=2,m
     f1(i)=0
     f2(i)=0
  end do

  ! Calculate fibonacci numbers until one is found that is greater
  ! than 10^999 (the first number with one thousand digits).
  i=2
  do while (f1(m) <= magic .and. f2(m) <= magic)
     if (mod(i,2) == 0) then
        call add(f1,f2)
     else
        call add(f2,f1)
     end if
     i=i+1
  end do

  ! Report result
  if (mod(i,2) == 1) then
     print *, f1
  else
     print *, f2
  end if
  print *, i

contains
  subroutine add(f1,f2)
    implicit none
    integer*16, parameter    :: mask=lshift(1_16,64)-1
    integer*16, dimension(m) :: f1,f2
    integer*16               :: temp
    integer                  :: i

    do i=1,m
       f1(i)=f1(i)+f2(i)       ! add
       temp=rshift(f1(i),64)
       f1(i)=iand(f1(i),mask)  ! start carry ...
       if (i < m) then
          f1(i+1)=f1(i+1)+temp ! ... finish carry
       end if
    end do

  end subroutine add

end program problem25
