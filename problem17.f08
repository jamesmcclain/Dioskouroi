program problem17
  implicit none
  integer :: i,s=0

  do i=1,1000
     s=s+letters(i)
  end do
  
  print *, s

contains
  pure function letters(starting_n)
    integer, intent(in) :: starting_n
    integer             :: n,letters

    n=starting_n
    letters=0
    
1   select case (n)
    case (1:2,6,10)
       letters=letters+3 ! one, two, six, ten
    case (4:5,9)
       letters=letters+4 ! four, five, nine
    case (3,7:8)
       letters=letters+5 ! three, seven, eight
    case (11:12)
       letters=letters+6 ! eleven, twelve
    case (13:14)
       letters=letters+8 ! thirteen, fourteen
    case (15:16)
       letters=letters+7 ! fifteen, sixteen
    case (17)
       letters=letters+9 ! seventeen
    case (18:19)
       letters=letters+8 ! eighteen, nineteen
    case (20:29)
       letters=letters+6 ! twenty
       n=n-20
       go to 1
    case (30:39)
       letters=letters+6 ! thirty
       n=n-30
       go to 1
    case (40:49)
       letters=letters+5 ! forty
       n=n-40
       go to 1
    case (50:59)
       letters=letters+5 ! fifty
       n=n-50
       go to 1
    case (60:69)
       letters=letters+5 ! sixty
       n=n-60
       go to 1
    case (70:79)
       letters=letters+7 ! seventy
       n=n-70
       go to 1
    case (80:89)
       letters=letters+6 ! eighty
       n=n-80
       go to 1
    case (90:99)
       letters=letters+6 ! ninety
       n=n-90
       go to 1
    case (100:999)
       letters=letters+7 ! hundred
       select case (n / 100)
       case (1:2,6)
          letters=letters+3 ! one, two, six
       case (3,7:8)
          letters=letters+5 ! three, seven, eight
       case (4:5,9)
          letters=letters+4 ! four, five, nine
       end select
       n=mod(n,100)
       if (n /= 0) then
          letters=letters+3 ! and
          go to 1
       end if
    case (1000)
       letters=letters+11 ! one thousand
    end select
    
  end function letters
  
end program problem17
