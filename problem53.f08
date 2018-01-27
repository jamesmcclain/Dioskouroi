program problem53
  implicit none
  integer :: n, r, count

  count=0
  do n=23,100
     do r=1,n
        if (predicate(n,r)) count=count+1
     end do
  end do
  
  print *, count

contains

  function predicate(n,r)
    implicit none
    integer, intent(in) :: n, r
    integer             :: i
    double precision    :: temp
    logical             :: predicate

    predicate=.false.
    temp=1.0
    do i=1,r
       temp=temp*(n-(r-i)) ! gets larger
       temp=temp/(r-i+1)   ! gets smaller
       if (temp>1000000.0) then
          predicate=.true.
          exit
       end if
    end do
    
  end function predicate
    
end program problem53
