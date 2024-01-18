!C806 The specification-part of a BLOCK construct shall not contain a NAMELIST
!statement

   integer a, b

   block
     namelist /nlist/ a, b
   end block
end
