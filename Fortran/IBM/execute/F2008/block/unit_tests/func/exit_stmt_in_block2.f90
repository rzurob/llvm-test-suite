! When an EXIT statement that belongs to a non-DO construct is executed,
! it terminates any active loops contained within that construct, and 
! completes execution of that construct.

integer :: i
a : block
 do i = 1, 3
   if ( i .eq. 2) exit a
 end do
 stop 1
end block a
if (i .ne. 2) stop 2
end
