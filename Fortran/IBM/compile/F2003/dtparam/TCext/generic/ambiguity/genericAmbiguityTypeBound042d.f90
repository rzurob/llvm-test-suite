! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound042d.f
! opt variations: -ql

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : both are dummy procedures and they are both subroutines
!*
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

   type b1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass(a) :: twoargs1
         generic :: twoargs => twoargs1
   end type

   type, extends(b1) :: c1    ! (4)
      contains
         procedure, pass(a) :: twoargs2
         generic :: twoargs => twoargs2
   end type

   abstract interface
      subroutine firstsub(a, b)
         integer, intent(in) :: a, b
      end subroutine
   end interface

   abstract interface
      subroutine secondsub(a)
         import b1
         type(b1(4)), intent(in) :: a
      end subroutine
   end interface

   contains

      subroutine twoargs1(a,b)
         class(b1(4)), intent(in) :: a
         procedure(firstsub) :: b

      end subroutine

      subroutine twoargs2(a,b)
         class(c1(4)), intent(in) :: a
         procedure(secondsub) :: b

      end subroutine

end module

program genericAmbiguityTypeBound042d
end program
