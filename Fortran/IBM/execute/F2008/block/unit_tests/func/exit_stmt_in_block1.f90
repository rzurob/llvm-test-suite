! When an EXIT statement that belongs to a non-DO construct is executed,
! it terminates any active loops contained within that construct, and 
! completes execution of that construct.

a : block
 exit a
 stop 1
end block a 
end
