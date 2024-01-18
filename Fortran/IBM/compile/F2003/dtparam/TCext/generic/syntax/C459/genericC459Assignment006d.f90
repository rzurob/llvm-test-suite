! GB DTP extension using:
! ftcx_dtp -qk -qreuse=base /tstdev/F2003/generic/syntax/C459/genericC459Assignment006d.f
! opt variations: -qck -qnok -qreuse=none

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
!*  SECONDARY FUNCTIONS TESTED : with Assignment( = )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C459: base type is private, and child type is public
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: base(k1,n1)    ! (4,3)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: c
      contains
         generic, public :: assignment(=) => typetotype
         procedure, pass, private :: typetotype => btob
   end type

   type, extends(base) :: child    ! (4,3)
      integer(k1) :: i
      contains
         procedure, pass ::  ctoi
         generic, private :: assignment(=) => ctoi
   end type

   contains

   subroutine btob ( a , b )
      class(base(4,*)) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c

   end subroutine

   subroutine ctoi ( a , b )
      class(child(4,*)) :: a
      integer :: b
      intent(out) :: a
      intent(in) :: b

      a%i = b

   end subroutine

end module

program genericC459Assignment006d
end program
