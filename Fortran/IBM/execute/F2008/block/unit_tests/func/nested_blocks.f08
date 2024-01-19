a : block
 integer :: i, j
 i = 2
 j = 3
 b : block
   integer :: i
   i = 4
   if (i .ne. 4) error stop 1
   if (j .ne. 3) error stop 2
 end block b
 if (i .ne. 2) error stop 3
end block a
end
