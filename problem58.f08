program problem58
  use euler
  implicit none
  integer   :: len,total,numprime
  integer*8 :: temp

  total=1
  numprime=0

  do len=3,huge(len),2
     total=total+4

     temp=(len**2)-(len-1)
     if (is_prime(temp)) numprime=numprime+1
     temp=temp-(len-1)
     if (is_prime(temp)) numprime=numprime+1
     temp=temp-(len-1)
     if (is_prime(temp)) numprime=numprime+1

     if (real(numprime)/total < 0.10) go to 100
  end do

100 continue
  print *, len, total, numprime

end program problem58
