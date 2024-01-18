! Paper 13-283 proposes an interp for TS 291113 (C-interop) that allows
! a scalar actual argument corresponding to an assumed-type and assumed-size
! dummy arg.

  real r1(4, 4)
  r1 = 1.0
  call sub(r1)
  contains
    subroutine sub(arg)
      type(*) :: arg(4, *)
      print*, size(arg, 1)
    end
  end
