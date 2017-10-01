program problem42
  implicit none
  integer, parameter         :: n=1786,m=20
  character(m), dimension(n) :: words
  integer                    :: i,answer=0

  open(33,file='./data/problem42.dat')
  read(33,*) words

  do i=1,n
     if (is_triangle(number(words(i)))) answer=answer+1
  end do

  print *, answer

contains

  pure function number(word)
    character(*), intent(in) :: word
    integer                  :: number,i

    number=0
    do i=1,len_trim(word)
       number=number+(ichar(word(i:i+1))-ichar('A')+1)
    end do

  end function number

  pure function is_triangle(n)
    integer, intent(in) :: n
    double precision    :: x1,x2
    logical             :: is_triangle

    ! quadratic formula
    x1 = (-1 - sqrt(dble(1+8*n))) / 2
    x2 = (-1 + sqrt(dble(1+8*n))) / 2

    is_triangle=.false.
    if (x1 > 0 .and. (mod(x1,1.0)==0.0)) is_triangle=.true.
    if (x2 > 0 .and. (mod(x2,1.0)==0.0)) is_triangle=.true.

  end function is_triangle

end program problem42
