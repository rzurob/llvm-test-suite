program main
  use ISO_C_BINDING

  interface
    subroutine cfun(x) bind(c)
      use ISO_C_BINDING, ONLY : C_FUNPTR
      type(c_funptr),value:: x
    end subroutine cfun
  end interface
  type(c_funptr) :: cproc

  print*, "Fortran program started:"
  cproc = c_funloc(fproc)
  print*, "Moving to C..."
  call cfun(cproc)

  contains
  subroutine fproc(b) bind(c)
    integer(C_INT), intent(in), value :: b
    print*, "Printing in Fortran!"
    print*, "   the value of b is:", b
  end subroutine
end program main
