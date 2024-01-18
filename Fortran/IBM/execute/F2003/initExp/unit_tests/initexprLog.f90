!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : LOG intrinsic
!*
!* DESCRIPTION                : complex type; signed zero
!* ===================================================================

program main
  implicit none
  logical  precision_x8, precision_x16

  complex(4) :: c4=log((-1.0e0,-0.0e0)), c4a
  complex(8) :: c8=log((-1.0d0,-0.0d0)), c8a

  c4a = log((-1.0e0,-0.0e0))
  c8a = log((-1.0d0,-0.0d0))

  if (.not. precision_x8(c4, c4a)) error stop 1
  if (.not. precision_x16(c8, c8a)) error stop 2

  call sub()
end

@PROCESS XLF2003(NOSIGNDZEROINTR)
subroutine sub()
  implicit none
  logical  precision_x8, precision_x16

  complex(4) :: c4=log((-1.0e0,-0.0e0)), c4a
  complex(8) :: c8=log((-1.0d0,-0.0d0)), c8a

  c4a = log((-1.0e0,-0.0e0))
  c8a = log((-1.0d0,-0.0d0))

  if (.not. precision_x8(c4, c4a)) error stop 3
  if (.not. precision_x16(c8, c8a)) error stop 4
end subroutine
