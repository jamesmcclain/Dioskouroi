program problem75
  implicit none
  integer, parameter        :: limit=1500000
  integer, dimension(limit) :: counts
  integer                   :: m,n,k,a,b,c,L,answer

  do k=1,limit
     counts(k)=0
  end do
  answer=0

  do m=1,int(ceiling(sqrt(dble(limit))))
     do n=1,m-1
        if (.not. predicate(m,n)) cycle
        a=(m*m)-(n*n)
        b=2*m*n
        c=(m*m)+(n*n)
        if (a<1) cycle
        L=a+b+c
        k=1
        do while(k*L<=limit)
           counts(k*L)=counts(k*L)+1
           k=k+1
        end do
     end do
  end do

  do k=1,limit
     if (counts(k)==1) answer=answer+1
  end do

  print *,answer

contains

  pure function predicate(a,b)
    use euler
    implicit none
    integer, intent(in) :: a,b
    logical             :: predicate

    predicate=(gcd(int(a,8),int(b,8))==1 .and. (mod(a,2)==0 .or. mod(b,2)==0))

  end function predicate

end program problem75
