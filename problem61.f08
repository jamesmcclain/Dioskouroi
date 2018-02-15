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
    use euler
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
       if (is_triangular(i) .and. (.not. any(polygonal(1:depth)==3))) then
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

end program problem61
