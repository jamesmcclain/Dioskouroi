program problem32
  implicit none
  double precision         :: magic=987654321.0
  integer                  :: i,j,answer
  logical, dimension(9999) :: numbers

  do i=1,9999
     numbers(i)=.false.
  end do
  
  do i=1,ceiling(sqrt(magic))
     do j=i+1,ceiling(magic/i)
        if (ineligible(i,j)) exit
        if (pandigital(i,j)) then
           numbers(i*j)=.true.
        end if
     end do
  end do

  answer=0
  do i=1,9999
     if (numbers(i)) then
        answer=answer+i
     end if
  end do
  
  print *, answer

contains
  pure function digits(a)
    implicit none
    integer, intent(in) :: a
    integer             :: digits

    digits=ceiling(log10(dble(a)))
  end function digits
  
  pure function ineligible(a,b)
    implicit none
    integer, intent(in) :: a,b
    logical             :: ineligible

    ineligible=(digits(a)+digits(b)+digits(a*b) > 9)
  end function ineligible

  pure function pandigital(a,b)
    implicit none
    integer, intent(in)   :: a,b
    integer               :: i,j,k,temp
    integer, dimension(9) :: d
    logical               :: pandigital

    if (digits(a)+digits(b)+digits(a*b) /= 9) then
       pandigital=.false.
       go to 1
    end if

    do i=1,9
       d(i)=0
    end do

    i=a
    j=b
    k=a*b

    do while (i > 0)
       temp=mod(i,10)
       d(temp)=d(temp)+1
       i=i/10
    end do

    do while (j > 0)
       temp=mod(j,10)
       d(temp)=d(temp)+1
       j=j/10
    end do

    do while (k > 0)
       temp=mod(k,10)
       d(temp)=d(temp)+1
       k=k/10
    end do

    do i=1,9
       if (d(i) /= 1) then
          pandigital=.false.
          go to 1
       end if
    end do

    pandigital=.true.

1 end function pandigital

end program problem32
