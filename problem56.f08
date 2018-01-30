program problem56
  use fmzm
  implicit none
  integer            :: a,b
  integer            :: best,temp
  integer, parameter :: limit=100

  best=-1
  do a=1,limit-1
     do b=1,limit-1
        temp=digit_sum(to_im(a)**to_im(b))
        if (temp>best) best=temp
     end do
  end do

  print *, best

contains

  function digit_sum(n)
    use fmzm
    implicit none
    type(im), intent(in) :: n
    type(im)             :: temp
    integer              :: digit_sum

    temp=n
    digit_sum=0
    do while(temp>0)
       digit_sum=digit_sum+to_int(mod(temp,to_im(10)))
       temp=temp/to_im(10)
    end do
    
  end function digit_sum

end program problem56
