! Cannot have internal-subprogram-part in BLOCK construct

block
  contains
    subroutine sub
    end subroutine
end block
end
