program problem86
  implicit none
  integer, parameter :: goal=1000000
  integer            :: m,count

  count=0
  m=0
  do while (count<goal .and. m<2000)
     m=m+1
     count=count+score(m)
  end do

  print *, m, count

contains

  pure function score(m)
    implicit none
    integer, intent(in) :: m
    integer             :: a,b,c,score

    score=0
    ! do a=m,m
    a=m
    do b=1,a
       do c=1,b
          if (predicate(a,b,c)) score=score+1
       end do
    end do
    ! end do

  end function score

  pure function predicate(a,b,c)
    implicit none
    integer, intent(in) :: a,b,c
    real*16             :: path1,path2,path3,best_path
    logical             :: predicate

    path1=sqrt(dble((a+b)**2 + c**2))
    path2=sqrt(dble((a+c)**2 + b**2))
    path3=sqrt(dble((b+c)**2 + a**2))
    best_path=min(path1,min(path2,path3))

    predicate=(mod(best_path,1.0)==0.0)
    
  end function predicate

end program problem86
