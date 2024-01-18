! Paper 13-283 proposes an interp for TS 291113 (C-interop) that allows
! a scalar actual argument corresponding to an assumed-type and assumed-size
! dummy arg. This test case is langlvl check as this feature is only enabled
! when -qlanglvl=ts and the above

  real r1
  r1 = 1.0
  call sub(r1)
  contains
    subroutine sub(arg)
      type(*) :: arg(*)
      print*, rank(arg)
    end
  end
