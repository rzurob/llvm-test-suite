! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound045d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : deferred binding using same interface, and deferred binding in another type and optional dummy args
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module genericName

   type, abstract :: b1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure(firstsub), deferred, pass(a) :: twoargs1
   end type

   type, extends(b1), abstract :: c1    ! (20,4)
      contains
         procedure(secondsub), deferred, pass(a) :: twoargs2
         generic :: twoargs => twoargs2
   end type

   type, extends(c1) :: g1    ! (20,4)
      contains
         procedure, pass(a) :: twoargs1
         procedure, pass(a) :: twoargs2
         generic :: twoargs => twoargs1
   end type

   abstract interface

      subroutine firstsub(a, b)
         import b1
         class(b1(*,4)), intent(in) :: a
         class(b1(*,4)), intent(in) :: b
      end subroutine

      subroutine secondsub(a, b, c)
         import b1, c1
         class(c1(*,4)), intent(in) :: a
         class(b1(*,4)), intent(in) :: b, c
         optional :: c
      end subroutine

   end interface

   contains

      subroutine twoargs1(a, b)
         class(g1(*,4)), intent(in) :: a
         class(b1(*,4)), intent(in) :: b
      end subroutine

      subroutine twoargs2(a, b, c)
         class(g1(*,4)), intent(in) :: a
         class(b1(*,4)), intent(in) :: b, c
         optional :: c

      end subroutine


end module

program genericAmbiguityTypeBound045d
end program
