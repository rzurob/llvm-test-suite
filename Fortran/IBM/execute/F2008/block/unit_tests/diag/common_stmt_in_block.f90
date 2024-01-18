!C806 The specification-part of a BLOCK construct shall not contain a COMMON
!statement
  block
    integer i
    integer j
    common i, j
  end block
end 
