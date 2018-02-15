program problem61
  implicit none
  integer, dimension(6) :: numbers,polygonal
  logical               :: success

  success=.false.
  call search(1)
  print *, numbers, polygonal
  print *, sum(numbers)

contains

  recursive subroutine search(depth)
    implicit none
    integer, intent(in) :: depth
    integer             :: i,start,fin

    if (depth>6 .and. (mod(numbers(6),100) == numbers(1)/100)) success=.true.

    if (depth==1) then
       start=1010
       fin=9999
    else
       start=mod(numbers(depth-1),100)*100+10
       fin=mod(numbers(depth-1),100)*100+99
    end if

    do i=start,fin
       if (success) return
       numbers(depth)=i
       if (is_triangle(i) .and. (.not. any(polygonal(1:depth)==3))) then
          polygonal(depth)=3
          call search(depth+1)
       else if (is_square(i) .and. (.not. any(polygonal(1:depth)==4))) then
          polygonal(depth)=4
          call search(depth+1)
       else if (is_pentagonal(i) .and. (.not. any(polygonal(1:depth)==5))) then
          polygonal(depth)=5
          call search(depth+1)
       else if (is_hexagonal(i) .and. (.not. any(polygonal(1:depth)==6))) then
          polygonal(depth)=6
          call search(depth+1)
       else if (is_heptagonal(i) .and. (.not. any(polygonal(1:depth)==7))) then
          polygonal(depth)=7
          call search(depth+1)
       else if (is_octagonal(i) .and. (.not. any(polygonal(1:depth)==8))) then
          polygonal(depth)=8
          call search(depth+1)
       end if
    end do

  end subroutine search

  pure function is_triangle(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_triangle,plus,minus

    temp=(1 + sqrt(1.0 + 4*2*m))/2
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(1 - sqrt(1.0 + 4*2*m))/2
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_triangle=plus .or. minus

  end function is_triangle

  pure function is_square(m)
    implicit none
    integer, intent(in) :: m
    logical             :: is_square

    is_square=mod(sqrt(dble(m)),1.0)==0.0

  end function is_square

  pure function is_pentagonal(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_pentagonal,plus,minus

    temp=(1 + sqrt(1.0 + 4*3*2*m))/6
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(1 - sqrt(1.0 + 4*3*2*m))/6
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_pentagonal=plus .or. minus

  end function is_pentagonal

  pure function is_hexagonal(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_hexagonal,plus,minus

    temp=(1 + sqrt(1.0 + 4*2*m))/4
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(1 - sqrt(1.0 + 4*2*m))/4
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_hexagonal=plus .or. minus

  end function is_hexagonal
  
  pure function is_heptagonal(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_heptagonal,plus,minus

    temp=(3 + sqrt(9.0 + 4*5*2*m))/10
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(3 - sqrt(9.0 + 4*5*2*m))/10
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_heptagonal=plus .or. minus

  end function is_heptagonal

  pure function is_octagonal(m)
    implicit none
    integer, intent(in) :: m
    double precision    :: temp
    logical             :: is_octagonal,plus,minus

    temp=(2 + sqrt(4.0 + 4*3*m))/6
    plus=(temp>0) .and. (mod(temp,1.0)==0.0)
    temp=(2 - sqrt(4.0 + 4*3*m))/6
    minus=(temp>0) .and. (mod(temp,1.0)==0.0)
    is_octagonal=plus .or. minus

  end function is_octagonal

end program problem61
