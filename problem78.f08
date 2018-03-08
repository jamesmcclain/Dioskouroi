program problem78
  use fmzm
  implicit none
  integer            :: i
  type(im)           :: temp,divisor,neg_one,zero,one,two
  integer, parameter :: memo_size=100000
  type(im)           :: memo(0:memo_size)

  neg_one=to_im(-1)
  zero=to_im(0)
  one=to_im(1)
  two=to_im(2)
  divisor=to_im(1000000)

  ! https://oeis.org/A000041
  do i=1,memo_size
     memo(i)=to_im(-1)
  end do
  memo(0)=one
  
  do i=1,memo_size
     temp=partitions(i)
     if (mod(temp,divisor)==zero) then
        print *, i
        exit
     end if
  end do

contains

  pure function plus_minus(n)
    implicit none
    integer, intent(in) :: n
    integer             :: plus_minus

    plus_minus=(-1)**((n-1)/2)
    
  end function plus_minus

  ! https://oeis.org/A001318
  function pentagonal(n)
    implicit none
    integer, intent(in) :: n
    integer             :: m,pentagonal

    if (mod(n,2)==1) then
       m=1
    else
       m=-1
    end if
    m=m*(n+1)/2
    pentagonal=m*(3*m-1)/2

  end function pentagonal

  ! https://en.wikipedia.org/wiki/Partition_(number_theory)#Generating_function
  function partitions(n)
    use fmzm
    implicit none
    integer, intent(in) :: n
    integer             :: i
    type(im)            :: partitions

    if (n<0) then
       partitions=zero
       return
    else if (n==0) then
       partitions=one
       return
    else if (memo(n)>neg_one) then
       partitions=memo(n)
       return
    else
       partitions=zero
       i=1
       do while (pentagonal(i)<=n)
          partitions=partitions+to_im(plus_minus(i))*memo(n-pentagonal(i))
          i=i+1
       end do
       memo(n)=partitions
       return
    end if

  end function partitions

end program problem78
