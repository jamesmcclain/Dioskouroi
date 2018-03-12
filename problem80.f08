program problem80
  use fmzm
  implicit none
  type(fm) :: one,zeros
  type(im) :: temp,ten
  integer  :: i,answer

  call fm_set(1000)

  one=to_fm(10.)**100
  zeros=to_fm(10.0)**200
  ten=to_im(10)

  answer=0
  do i=0,100
     if (predicate(i)) answer=answer+score(i)
  end do

  print *, answer

contains

  function score(n)
    use fmzm
    implicit none
    integer, intent(in) :: n
    type(im)            :: temp1
    real*8              :: temp2
    integer             :: score

    temp1=to_im(sqrt(n*zeros))

    ! Ensure exactly 100 digits
    temp2=sqrt(dble(n))
    do while(temp2>1.0)
       temp2=temp2/10.0
       temp1=temp1/ten
    end do

    ! Add score
    score=0
    do while(temp1>0)
       score=score+to_int(mod(temp1,ten))
       temp1=temp1/ten
    end do

  end function score
  
  pure function predicate(n)
    implicit none
    integer, intent(in) :: n
    logical             :: predicate

    predicate=mod(sqrt(dble(n)),1.0)/=0.0

  end function predicate

end program problem80
