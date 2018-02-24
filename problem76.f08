program problem76
  implicit none
  integer :: i
  integer, dimension(100) :: memo

  ! https://oeis.org/A000041
  do i=1,100
     memo(i)=-1
  end do
  memo(1:10)=(/ 1, 2, 3, 5, 7, 11, 15, 22, 30, 42 /)
  
  print *, partitions(100)-1

contains

  pure function plus_minus(n)
    implicit none
    integer, intent(in) :: n
    integer             :: plus_minus

    plus_minus=(-1)**((n-1)/2)
    
  end function plus_minus

  ! https://oeis.org/A001318
  pure function pentagonal(n)
    implicit none
    integer, intent(in) :: n
    integer             :: pentagonal,m

    if (mod(n,2)==1) then
       m=1
    else
       m=-1
    end if
    m=m*(n+1)/2
    pentagonal=m*(3*m-1)/2

  end function pentagonal

  ! https://en.wikipedia.org/wiki/Partition_(number_theory)#Generating_function
  recursive function partitions(n) result(r)
    implicit none
    integer, intent(in) :: n
    integer             :: r,i

    if (n<0) then
       r=0
       return
    else if (n==0) then
       r=1
       return
    else if (memo(n)>-1) then
       r=memo(n)
       return
    else
       r=0
       i=1
       do while (pentagonal(i)<=n)
          r=r+plus_minus(i)*partitions(n-pentagonal(i))
          ! print *,r,i,plus_minus(i),pentagonal(i),partitions(n-pentagonal(i))
          i=i+1
       end do
       memo(n)=r
       return
    end if

  end function partitions

end program problem76
