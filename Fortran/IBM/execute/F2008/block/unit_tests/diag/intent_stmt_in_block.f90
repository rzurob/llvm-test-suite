!C806 The specification-part of a BLOCK construct shall not contain an INTENT
!statement

  contains
    subroutine foo (a)
       block
        intent(in) a
       end block
    end subroutine
end
