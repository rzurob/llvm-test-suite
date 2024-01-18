! Adapted from Rice Univ test case:
! tests/translator/explicit_shape_coarray/1d_local/arg_test.caf
program arg_test
   integer, save:: A(4)[*]
   interface 
      subroutine foo(A)
         integer:: A(3)[*]
      end subroutine foo
    end interface 

   integer:: size,rank,partner

   size = NUM_IMAGES()
   rank = THIS_IMAGE()
   partner = size-rank+1

   A(1) = partner

   SYNC ALL

   if (rank .eq. 1) then
       if (A(1) .eq. partner) then 
          print *," OK: step 1 of 3"
       else
          print *, "Node 1: in caller,A(1) should be ",partner, " but we have A(1)= ", A(1)
       endif 
   end if 

   SYNC ALL

   call foo(A(:))

   SYNC ALL

   if (rank .eq. 1) then
     if (A(1) .eq. 1) then 
        print *, " OK: step 3 of 3"
      else 
        print *, "Node 1: in caller, A(1) should be value reseted in the callee (0) instead of ",A(1)
    end if
   end if 
end


subroutine foo(A)
  integer A(3)[*]
  integer rank

  rank = THIS_IMAGE()

  A(1) =  rank

  if (rank .eq. 1)  then 
    if (A(1) .eq. 1) then 
       print *," OK: step 2 of 3"
    else 
      print *,"Node 1: callee reset A(1)[1] = 1"
   end if 
 end if

end
