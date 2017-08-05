program problem14
  implicit none
  integer, parameter :: limit=1000000,stack_limit=100000000,cache_limit=100000000
  integer*8          :: i,temp,best,longest=-1
  integer*8, allocatable, dimension(:) :: cache,stack

  allocate(stack(stack_limit))
  allocate(cache(cache_limit))
  forall (i=1:cache_limit) cache(i)=-1
  cache(1)=1

  do i=1,limit
     temp=collatz(i)
     if (temp > longest) then
        longest=temp
        best=i
     end if
  end do
  
  print *, best

  deallocate(cache)
  deallocate(stack)
  
contains
  function collatz(starting_n)
    implicit none
    integer*8, intent(in) :: starting_n
    integer*8             :: sp,n,collatz

    n=starting_n

    do sp=1,stack_limit ! wind stack
       stack(sp)=n
       if (n <= cache_limit) then ! cannot assume short-circuit evaluation of conditional
          if (cache(n) > -1) then ! found cached value
             collatz=cache(n)-1
             go to 1
          end if
       end if
       if (mod(n,2) == 0) then    ! uncached even value
          n=n/2
       else                       ! uncached odd value
          n=3*n+1
       end if
    end do
    error stop 'stack limit exceeded'
    
1   do sp=sp,1,-1 ! unwind stack
       n=stack(sp)
       collatz=collatz+1
       if (n <= cache_limit) then
          cache(n)=collatz
       end if
    end do

  end function collatz

end program problem14
