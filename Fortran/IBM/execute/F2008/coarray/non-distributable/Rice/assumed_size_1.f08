! Adopted from Rice Univ test case:
! tests/regression/assumed-size-coarray-1
program assumed_size_coarray_test
    integer, save ::A(4)[*]
    interface
       subroutine foo(A)
          integer:: A(1:*)[*]
       end subroutine foo
    end interface

    integer:: size,rank,partner

    size = NUM_IMAGES()
    rank = THIS_IMAGE()

    partner = size-rank + 1

    A(1) = rank

    SYNC ALL

    if (rank .eq. 0) then
       A(1) =A(1)[partner]
       if (A(1) .eq. partner) then
          print *,"OK: Assumed size coarray test:step 1 of 3 in the caller."
       else
          print *, "Assumed size coarray test fail: in caller,A(1) should be ",partner, " but we have A(1)= ", A(1)
       endif
    end if

    SYNC ALL

    call foo(A(:))

    SYNC ALL

    if (rank .eq. 1) then
      if (A(1) .eq. size) then
         print *, "OK: Assumed size coarray test: step 3 of 3 back to the caller"
       else
         print *, "Assumed size coarray test failed: in caller, A(1) should be value reset in the callee instead of ",A(1)
     end if
    end if
end

subroutine foo(A)
    integer A(1:*)[*]
    integer rank, size

    rank = THIS_IMAGE()
    size = NUM_IMAGES()

    if (rank .eq. 1)  then
      A(1) =  A(1)[size]
      if (A(1) .eq. size) then
         print *,"OK: Assumed size coarray test: step 2 of 3 in the callee"
      else
        print *,"Assumed size coarray test failed: callee reset A(1)[1] = ",size
      end if
    end if
end
