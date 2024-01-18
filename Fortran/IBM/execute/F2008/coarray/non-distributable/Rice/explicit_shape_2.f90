! Adapted from Rice Univ. test case
! tests/Attic/translator/test3/test3.caf
program  test3
   integer,save::A(4,4)[*]
   interface
      subroutine foo(A)
         integer:: A[*]
      end subroutine foo
    end interface

   integer:: size,rank,partner

   size = NUM_IMAGES()
   rank = THIS_IMAGE()

   partner = size-rank + 1

   A(3,3) = rank

   SYNC ALL

   if (rank .eq. 1) then
       A(3,3) =A(3,3)[partner]
       if (A(3,3) .eq. partner) then
          print *,"test3 OK: step 1 of 3"
       else
          print *, "test3 failed: in caller,A(1) should be ",partner, " but we have A(3,3)= ", A(3,3)
       endif
   end if

   SYNC ALL

   call foo(A(3,3))

   SYNC ALL

   if (rank .eq. 1) then
     if (A(3,3) .eq. 2) then
        print *, "test3 OK: step 3 of 3"
      else
        print *, "test3 failed: in caller, A(3,3) should be value reseted in the callee (2) instead of ", A(3,3)
    end if
   end if
end program

subroutine foo(A)
  integer A[*]
  integer rank

  rank = THIS_IMAGE()

  if (rank .eq. 1)  then
    A =  A[2]
    if (A .eq. 2) then
      print *,"test3 OK: step 2 of 3"
    else
      print *,"test3 faile: caller reset A[1] = 2"
    end if
 end if
end
