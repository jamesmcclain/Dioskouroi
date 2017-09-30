program problem41
  use euler
  implicit none
  integer, parameter        :: limit=9
  integer, dimension(limit) :: c, A
  integer                   :: i,temp,n,answer=0

  do n=limit,2,-1

     ! Start of Heap's Algorithm
     ! Source: https://en.wikipedia.org/wiki/Heap%27s_algorithm
     do i=1,n
        c(i)=0
        A(i)=i
     end do

     temp=number(A,n)
     if (is_prime(int8(temp)) .and. temp>answer) answer=temp

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
           temp=number(A,n)
           if (is_prime(int8(temp)) .and. temp>answer) answer=temp
           c(i+1)=c(i+1)+1
           i=0
        else
           c(i+1)=0
           i=i+1
        end if

        ! End of Heap's Algorithm
     end do

     if (answer>0) exit

  end do

  print *, answer

contains
  pure function number(A,n)
    implicit none
    integer, dimension(limit), intent(in) :: A
    integer, intent(in)                   :: n
    integer                               :: number,i

    number=0

    do i=1,n
       number=number*10
       number=number+A(i)
    end do

  end function number

end program problem41
