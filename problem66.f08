program problem66
  use fmzm
  implicit none
  integer, parameter      :: len=100
  integer, dimension(len) :: array
  integer                 :: D, bestD
  type(im)                :: bestx,temp

  call fm_set(200)

  bestD=-1
  bestx=to_im(-1)
  do D=1,1000
     if (mod(sqrt(dble(D)),1.0) == 0.0) cycle
     call cf(D,array)
     temp=solve(D,array)
     if (temp>bestx) then
        bestx=temp
        bestD=D
     end if
  end do

  print *, bestD, im_format('i50', bestx)

contains

  subroutine cf(D,array)
    use fmzm
    implicit none
    integer, intent(in)     :: D
    integer, dimension(len) :: array
    integer                 :: i
    type(fm)                :: temp,one

    one=to_fm(1)
    temp=mod(sqrt(to_fm(D)),one)
    do i=1,len
       temp=1.0/temp
       array(i)=int(temp)
       temp=mod(temp,one)
    end do

  end subroutine cf

  ! Reference: https://en.wikipedia.org/wiki/Pell%27s_equation
  function solve(D,array)
    use fmzm
    implicit none
    integer, intent(in)                 :: D
    integer, dimension(len), intent(in) :: array
    type(im)                            :: numerator,denomenator,temp,solve
    integer                             :: i,n

    solve=to_im(-1)
    do n=1,len
       numerator=to_im(1)
       denomenator=to_im(array(n))

       do i=n-1,1,-1
          numerator=to_im(array(i))*denomenator+numerator
          temp=numerator
          numerator=denomenator
          denomenator=temp
       end do

       temp=to_im(sqrt(dble(D)))
       numerator=numerator+temp*denomenator

       if (numerator**2 - D*denomenator**2 == 1) then
          solve=numerator
          go to 100
       end if
    end do

100 continue

  end function solve

end program problem66
