program problem38
  use euler
  implicit none
  integer               :: i,j,k,answer=-1
  integer, dimension(9) :: products,lengths

  do i=2,9
     j=1
     do while (.true.)
        forall (k=1:i)
           products(k)=k*j
           lengths(k)=number_length(int8(products(k)))
        end forall
        k=sum(lengths(1:i))
        if (k>9) exit
        if (k==9) then
           if (pandigital(products,i)) then
              answer=max(answer,to_number(products,i))
           end if
        end if
        j=j+1
     end do
  end do

  print *, answer

contains
  pure function to_number(ns,i)
    implicit none
    integer, dimension(9), intent(in) :: ns
    integer,intent(in)                :: i
    integer                           :: to_number,j

    to_number=0
    do j=1,i
       to_number=(to_number*10**(number_length(int8(ns(j)))))+ns(j)
    end do

  end function to_number

  pure function pandigital(ns,i)
    implicit none
    integer, dimension(9), intent(in) :: ns
    integer, intent(in)               :: i
    integer, dimension(10)            :: found
    integer                           :: j,product,index
    logical pandigital

    pandigital=.true.

    forall (j=1:10) found(j)=0

    do j=1,i
       product=ns(j)
       do while (product>0)
          index=mod(product,10)+1 ! turn modulus into index
          if (found(index)>0) then
             pandigital=.false.
             go to 1
          end if
          found(index)=1
          product=product/10
       end do
    end do

    if ((found(0+1) /= 0) .or. (sum(found(2:10)) /= 9)) then
       pandigital=.false.
    end if

1 end function pandigital

end program problem38
