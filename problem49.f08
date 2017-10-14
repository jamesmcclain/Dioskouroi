program problem49
  use euler
  implicit none
  integer :: i,j,k

  do i=1000,9999
     if (.not. is_prime(int8(i))) cycle

     do j=i+1,9999
        if (.not. is_prime(int8(j))) cycle
        if (.not. permutations(i,j)) cycle

        do k=j+1,9999
           if (.not. is_prime(int8(k))) cycle
           if (.not. permutations(j, k)) cycle
           if (j - i == k - j) print *, i, j, k
        end do

     end do

  end do
  
contains

  pure function permutations(a,b)
    implicit none
    integer, intent(in)    :: a,b
    integer, dimension(10) :: a_ar, b_ar
    integer                :: i,temp,a_curr,b_curr
    logical                :: permutations

    do i=1,10
       a_ar(i)=0
       b_ar(i)=0
    end do

    a_curr=a
    b_curr=b

    do i=1,4
       temp=mod(a_curr,10)+1
       a_ar(temp)=a_ar(temp)+1

       temp=mod(b_curr,10)+1
       b_ar(temp)=b_ar(temp)+1

       a_curr=a_curr/10
       b_curr=b_curr/10
    end do

    permutations=all(a_ar == b_ar,1)

  end function permutations

end program problem49
  

