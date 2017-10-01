program problem43
  implicit none
  integer, parameter     :: n=10
  integer                :: i,temp
  integer*8              :: answer=0
  integer, dimension(10) :: c,A

  ! Start of Heap's Algorithm
  ! Source: https://en.wikipedia.org/wiki/Heap%27s_algorithm
  do i=0,n-1
     c(i)=0
     A(i)=i
  end do

  if (predicate(A)) answer=answer+number(A,n)

  i=0
  do while (i<n)
     if (c(i+1)<i) then
        if (mod(i,2)==0) then
           temp=A(0+1)
           A(0+1)=A(i+1)
           A(i+1)=temp
        else
           temp=A(c(i+1)+1)
           A(c(i+1)+1)=A(i+1)
           A(i+1)=temp
        end if
        if (predicate(A)) answer=answer+number(A,n)
        c(i+1)=c(i+1)+1
        i=0
     else
        c(i+1)=0
        i=i+1
     end if

  end do
  ! End of Heap's Algorithm

  print *, answer

contains

  pure function number(A,n)
    implicit none
    integer, dimension(*), intent(in) :: A
    integer, intent(in)               :: n
    integer                           :: i
    integer*8                         :: number

    number=0

    do i=1,n
       number=number*10
       number=number+A(i)
    end do

  end function number

  pure function predicate(A)
    integer, dimension(n), intent(in) :: A
    logical                           :: predicate

    predicate=.true.

    if (mod(number(A(2:4),3),2) /= 0) then
       predicate=.false.
       goto 1
    end if
    if (mod(number(A(3:5),3),3) /= 0) then
       predicate=.false.
       goto 1
    end if
    if (mod(number(A(4:6),3),5) /= 0) then
       predicate=.false.
       goto 1
    end if
    if (mod(number(A(5:7),3),7) /= 0) then
       predicate=.false.
       goto 1
    end if
    if (mod(number(A(6:8),3),11) /= 0) then
       predicate=.false.
       goto 1
    end if
    if (mod(number(A(7:9),3),13) /= 0) then
       predicate=.false.
       goto 1
    end if
    if (mod(number(A(8:10),3),17) /= 0) then
       predicate=.false.
       goto 1
    end if

1 end function predicate

end program problem43
