!C1278 A local variable of a pure subprogram, or of a BLOCK construct within a pure
!subprogram, shall not have the SAVE attribute.

pure subroutine sub
  block
    integer i
    static i
  end block
end
