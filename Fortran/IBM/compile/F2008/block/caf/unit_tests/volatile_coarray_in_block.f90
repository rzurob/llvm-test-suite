!C561 Within a BLOCK construct , the VOLATILE attribute shall not be specified
!for a coarray that is not a construct entity of that construct.

integer, save :: i[*]
  
block 
  volatile i
end block
end 
