program problem29
  implicit none
  integer, parameter                    :: a_limit=100,b_limit=100
  integer, parameter                    :: power=floor(log(dble(a_limit))/log(dble(2)))
  integer                               :: i,j,k,answer
  double precision                      :: temp
  logical, dimension(a_limit,b_limit*6) :: found

  do i=1,a_limit
     do j=1,b_limit*power
        found(i,j) = .false.
     end do
  end do

  do i=2,a_limit
     ! Filter out powers of earlier numbers
     do j=2,i-1
        temp=log(dble(i))/log(dble(j))
        if (mod(temp,1.0_8) == 0.0_8) then
           go to 1
        end if
     end do

     ! Mark found numbers
     do j=2,b_limit
        ! Largest exponent of this number that is less than the largest a
        do k=1,floor(log(dble(a_limit))/log(dble(i)))
           found(i,j*k) = .true.
        end do
     end do
1 end do

  answer=0
  do i=1,a_limit
     do j=1,power*b_limit
        if (found(i,j)) then
           answer=answer+1
        end if
     end do
  end do

  print *, answer

end program problem29
