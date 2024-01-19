! Implicit typing is not affected by BLOCK constructs.

implicit integer (a)
block
 a = 1.0
 print *, a
end block
end
