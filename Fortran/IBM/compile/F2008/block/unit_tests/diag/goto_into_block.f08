! Cannot transfer control into a block using goto

goto 10
block
10 print *, "should not be here"
end block
end
