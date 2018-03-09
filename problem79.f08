program problem79
  implicit none
  integer, parameter          :: uniques=33
  integer, dimension(uniques) :: data
  integer                     :: i,num

  open(33,file='./data/problem79-unique.dat')
  read(33,*) data

  do i=1,huge(i)
     if (predicate1(i)) exit
  end do

  print *, i

contains

  pure function predicate1(n)
    implicit none
    integer, intent(in)         :: n
    integer                     :: i
    logical, dimension(uniques) :: bits
    logical                     :: predicate1

    forall (i=1:uniques)
       bits(i)=predicate2(n,data(i))
    end forall
    predicate1=all(bits)

  end function predicate1

  pure function predicate2(n,m)
    implicit none
    integer, intent(in) :: n,m
    integer             :: a,b,temp1,temp2,count
    logical             :: predicate2

    a=n
    b=m
    count=0
    predicate2=.false.

    temp2=mod(b,10)
    do while (a>0)
       temp1=mod(a,10)
       if (temp1==temp2) then
          b=b/10
          temp2=mod(b,10)
          count=count+1
          if (count==3) then
             predicate2=.true.
             return
          end if
       end if
       a=a/10
    end do

  end function predicate2

end program problem79
