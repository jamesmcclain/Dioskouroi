program problem78
  use fmzm
  implicit none
  integer                        :: i
  type(im)                       :: temp,divisor,neg_one,zero,one,two
  integer, parameter             :: memo_size=1000
  type(im), dimension(memo_size) :: memo

  neg_one=to_im(-1)
  zero=to_im(0)
  one=to_im(1)
  two=to_im(2)
  divisor=to_im(1000)

  ! ...
  ! https://oeis.org/A000041
  do i=1,memo_size
     memo(i)=to_im(-1)
  end do
  memo(1)=to_im(1)
  memo(2)=to_im(2)
  memo(3)=to_im(3)
  memo(4)=to_im(5)
  memo(5)=to_im(7)
  memo(6)=to_im(11)
  memo(7)=to_im(15)
  memo(8)=to_im(22)
  memo(9)=to_im(30)
  memo(10)=to_im(42)
  
  do i=1,memo_size
     temp=partitions(to_im(i))
     if (mod(temp,divisor)==zero) then
        print *, i
     end if
  end do

contains

  function plus_minus(n)
    use fmzm
    implicit none
    type(im), intent(in) :: n
    type(im)             :: plus_minus

    plus_minus=(-1)**((n-1)/2)
    
  end function plus_minus

  ! https://oeis.org/A001318
  function pentagonal(n)
    use fmzm
    implicit none
    type(im), intent(in) :: n
    type(im)             :: pentagonal,m

    if (mod(n,two)==one) then
       m=1
    else
       m=-1
    end if
    m=m*(n+1)/2
    pentagonal=m*(3*m-1)/2

  end function pentagonal

  ! https://en.wikipedia.org/wiki/Partition_(number_theory)#Generating_function
  recursive function partitions(n) result(r)
    use fmzm
    implicit none
    type(im), intent(in) :: n
    type(im)             :: r,i

    if (n<zero) then
       r=0
       return
    else if (n==zero) then
       r=1
       return
    else if (memo(to_int(n))>neg_one) then
       r=memo(to_int(n))
       return
    else
       r=zero
       i=one
       do while (pentagonal(i)<=n)
          r=r+plus_minus(i)*partitions(n-pentagonal(i))
          i=i+1
       end do
       memo(to_int(n))=r
       return
    end if

  end function partitions

end program problem78
