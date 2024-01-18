! Adapted from Rice Univ test case:
! tests/regression/trans-scalar_coarray_arg
program arg_test

   interface
      subroutine foo(A)
         integer:: A[*]
      end subroutine foo
    end interface

   integer, save :: A(10)[*]

   integer:: size,rank,partner

   size = NUM_IMAGES()
   rank = THIS_IMAGE()

   partner = size-rank + 1

   A(1) = partner

   SYNC ALL

   call foo(A(1))

   SYNC ALL

   if (rank .eq. 1) then
     if (A(1) .eq. A(1)[size]) then
        print *, "OK: the value is reset in the callee (remote read)"
      else
        print *, "ERROR: Node 1: in caller, A(1) should be value reseted in the callee instead of ",A(1)
    end if
   end if
end

subroutine foo(A)
  integer A[*]
  integer rank
  integer iszie

  isize = NUM_IMAGES()
  rank = THIS_IMAGE()

  if (rank .eq. 1)  then
    if (A .eq. isize -rank + 1) then
       print *, "OK: in the callee, get the scalar coarray."
       A = A[isize]
    else
      print *,"Node 1: A should be ", isize-rank+1, "but we have ", A;
   end if
 end if

end
