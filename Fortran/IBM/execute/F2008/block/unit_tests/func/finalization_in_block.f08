!finalization upon exiting BLOCK construct

module m
type dt
  integer :: i
  contains
  final :: dtfinal
end type
  contains
  subroutine dtfinal(arg)
    type(dt) :: arg
    print *, arg%i, "finalised"
  end subroutine
end module

use m
block
type(dt) j
j%i = 12
end block

end
