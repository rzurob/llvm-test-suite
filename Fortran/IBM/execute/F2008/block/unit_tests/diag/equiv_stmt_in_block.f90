!C806 The specification-part of a BLOCK construct shall not contain an
!EQUIVALENCE statement
  block
    integer i
    integer j

    equivalence (i, j)
  end block

end 
