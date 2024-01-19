! Adopted from Rice Univ test case:
! tests/regression/get-2
program test9
    integer,save::A(9)[*]
    interface
      subroutine foo(A)
         integer:: A(3,*)[*]
      end subroutine foo
     end interface

    integer:: size,rank,partner

    size = NUM_IMAGES()
    rank = THIS_IMAGE()

    partner = size-rank + 1

    A(9) = rank

    SYNC ALL

    if (rank .eq. 1) then
       A(9) =A(9)[partner]
       if (A(9) .eq. partner) then
          print *,"OK: In caller, remote read (step 1 of 3)"
       else
          print *,"failed: in caller,A(9) should be ",partner, " but we have A(9)= ", A(9)
       endif
    end if

    SYNC ALL

    call foo(A(:))

    SYNC ALL

    if (rank .eq. 1) then
     if (A(9) .eq. 2) then
        print *, "OK: in caller, after reset value in callee (step 3 of 3)"
      else
        print *, "failed: in caller, A(9) should be value reseted in the callee (2) instead of ",A(9)
    end if
end

subroutine foo(A)
    integer A(3,*)[*]
    integer rank

    rank = THIS_IMAGE()

    if (rank .eq. 1)  then
      A(3,3) =  A(3,3)[2]
      if (A(3,3) .eq. 2) then
         print *,"OK: in callee, on node 1, remote read from node 1 (step 2 of 3)"
      else
         print *,"Failed: callee should be reset A(3,3)[1] = 2"
      end if
    end if

end
