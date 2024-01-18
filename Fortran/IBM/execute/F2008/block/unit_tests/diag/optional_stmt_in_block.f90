!C806 The specification-part of a BLOCK construct shall not contain an OPTIONAL
!statement
  
  contains
    subroutine foo (a)
       block
        optional a
       end block
    end subroutine
end 
