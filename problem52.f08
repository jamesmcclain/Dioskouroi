program problem52
  implicit none
  integer*8 :: i

  do i = 1,huge(i)
     if (predicate(i)) then
        exit
     end if
  end do

  print *, i

contains

  pure function score(n)
    implicit none
    integer*8, intent(in)             :: n
    integer*8                         :: temp,score
    integer, parameter, dimension(10) :: primes = [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]

    score=1
    temp=n
    do while (temp > 0)
       score=score*primes(mod(temp,10)+1)
       temp=temp/10
    end do
    
  end function score

  pure function predicate(n)
    implicit none
    integer*8, intent(in) :: n
    integer*8             :: temp1
    logical, dimension(5) :: temp2
    logical               :: predicate

    temp1=score(n)
    temp2=(/ (score(n*i) == temp1, i = 2, 6) /)
    predicate=all(temp2)

  end function predicate

end program problem52
