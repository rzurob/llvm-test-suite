!C806 The specification-part of a BLOCK construct shall not contain an VALUE
!statement
  
  contains
    subroutine foo (a)
       block
         value a
       end block
    end subroutine
end 
