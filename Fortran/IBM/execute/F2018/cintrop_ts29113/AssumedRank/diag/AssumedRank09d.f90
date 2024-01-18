!!! non-assumed length parameter must be match for assumed-rank dummy.
  interface
    subroutine check_scalar(arg1)
      character(1) :: arg1(..)
    end subroutine
  end interface

  character(4) :: char
  call check_scalar(char)
end

