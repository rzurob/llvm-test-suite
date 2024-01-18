!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : SQRT intrinsic
!*
!* DESCRIPTION                : complex type; signed zero
!* ===================================================================

program main
  implicit none
  logical precision_x8, precision_x16
  complex(4) :: c4=sqrt((-1.0e0,-0.0e0)), c4a
  complex(8) :: c8=sqrt((-1.0d0,-0.0d0)), c8a

  c4a = sqrt((-1.0e0,-0.0e0))
  c8a = sqrt((-1.0d0,-0.0d0))

  print *, c4, c4a
  print *, c8, c8a

  call sub()
end program

@PROCESS XLF2003(NOSIGNDZEROINTR)
subroutine sub()
  implicit none
  logical precision_x8, precision_x16
  complex(4) :: c4=sqrt((-1.0e0,-0.0e0)), c4a
  complex(8) :: c8=sqrt((-1.0d0,-0.0d0)), c8a

  c4a = sqrt((-1.0e0,-0.0e0))
  c8a = sqrt((-1.0d0,-0.0d0))

  print *, c4, c4a
  print *, c8, c8a
end subroutine

