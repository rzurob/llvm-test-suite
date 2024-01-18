! Adopted from Rice Univ test case:
! tests/regression/assumed-size-coarray-2
program assumed_size_coarray_test_2
    integer, save ::A(4)[*]
    interface 
      subroutine foo(A)
         integer:: A(*)[*]
      END Subroutine foo
     end interface 

    integer:: size,rank,partner

    size = NUM_IMAGES()
    rank = THIS_IMAGE()

    partner = size-rank + 1
    A(1) = rank

    SYNC ALL

    if (rank .eq. size) then
        A(1)[1] =A(1)
    end if

    SYNC ALL

    if (rank .eq. 1) then
        if (A(1) .eq. size) then 
           print *,"OK: Assumed_size_coarray_test_2 : step 1 of 3, in caller, assgin value locally"
        else
           print *,"Assumed_size_coarray_test_2 failed: in caller,A(1) should be ",partner, " but we have A(1)= ", A(1)
        endif 
    end if 

    SYNC ALL 

    call foo(A(:))

    SYNC ALL 

    if (rank .eq. 1) then
      if (A(1) .eq. 2) then 
         print *, "OK: Assumed_size_coarray_test_2 : step 3 of 3, back to the caller, check the remote write."
       else 
         print *, "Assumed_size_coarray_test_2 failed: in caller, A(1) should be value reseted in the callee (2) instead of ",A(1)
     end if
    end if 
end


subroutine foo(A)
    integer A(*)[*]
    integer rank

    rank = THIS_IMAGE()
    A(1) = rank

    SYNC ALL
    if (rank .eq. 2) then
      A(1)[1] =  A(1)
    end  if

    SYNC ALL

    if (rank .eq. 1)  then 
      if (A(1) .eq. 2) then 
         print *,"OK: Assumed_size_coarray_test_2 : step 2 of 3, remote write in callee"
      else 
        print *,"Assumed_size_coarray_test_2 failed: callee reset A(1)[1] = 2"
      end if 
    end if
end
