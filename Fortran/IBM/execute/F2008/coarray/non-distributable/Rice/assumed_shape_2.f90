! Adopted from Rice Univ test case:
! tests/regression/put-3
program test21
    integer,save::A(4)[*]
    interface 
      subroutine foo(A)
         integer:: A(:)[*]
      end subroutine foo
    end interface 

    integer:: size,rank,partner, rankd

    size = NUM_IMAGES()
    rank = THIS_IMAGE()

    A(1) = rank

    SYNC ALL

    if (rank .eq. size) then 
       A(1)[1] =A(1)[size]
    end if 

    SYNC ALL

    if (rank .eq. 1) then
       if (A(1) .eq. size) then 
          print *,"OK: step 1 of 3"
       else
          print *, "failed: in caller,A(1) should be ",size, " but we have A(1)= ", A(1)
       endif 
    end if 

    SYNC ALL

    call foo(A(:))

    SYNC ALL

    if (rank .eq. 1) then
      if (A(1) .eq. 2) then 
         print *, "OK: step 3 of 3"
       else 
         print *, "failed: in caller, A(1) should be value reseted in the callee (2) instead of ",A(1)
     end if
    end if 
end


subroutine foo(A)
    integer A(:)[*]
    integer rank, size

    size = NUM_IMAGES()
    rank = THIS_IMAGE()

    A(1) = rank

    SYNC ALL

    if (rank .eq. 2) then
      A(1)[1] =  A(1)
    end  if

    SYNC ALL

    if (rank .eq. 1)  then 
      if (A(1) .eq. 2) then 
         print *,"OK: step 2 of 3"
      else 
        print *,"failed: callee reset A(1)[1] = 2"
      end if 
    end if

end
