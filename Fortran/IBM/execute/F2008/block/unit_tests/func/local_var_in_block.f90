! Actions on a variable local to a BLOCK construct do not act any variable of
! the same name outside the construct.

integer :: i
i = 3
block
  integer :: i
  i = 4
  if (i .ne. 4) error stop 1
end block
if (i .ne. 3) error stop 2
end
