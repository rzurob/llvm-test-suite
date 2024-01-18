! Adopted from Rice Univ test case:
! tests/regression/get-3
program test20
    integer, save ::A(4)[*]
    interface 
      subroutine foo(A)
         integer:: A(:)[*]
      end subroutine foo
    end interface 

    integer:: size,rank,partner

    size = NUM_IMAGES()
    rank = THIS_IMAGE()

    partner = size-rank + 1

    A(1) = rank

    SYNC ALL

    if (rank .eq. 1) then
       A(1) =A(1)[partner]
       if (A(1) .eq. partner) then 
          print *,"OK: step 1 of 3"
       else
          print *, "failed: in caller,A(1) should be ",partner, " but we have A(1)= ", A(1)
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
end program


subroutine foo(A)
    integer A(:)[*]
    integer rank

    rank = THIS_IMAGE()


    if (rank .eq. 1)  then 
      A(1) =  A(1)[2]
      if (A(1) .eq. 2) then 
         print *,"OK: step 2 of 3"
      else 
        print *,"failed: callee reset A(1)[0] = 0"
     end if 
   end if

end
