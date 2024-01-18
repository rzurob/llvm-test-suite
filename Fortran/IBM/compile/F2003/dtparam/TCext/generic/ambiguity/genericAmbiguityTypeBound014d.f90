! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound014d.f
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
!*  DESCRIPTION                : two argument with pass-arg to be first arg specified (for generic-name, operator, and assignment tb)
!*                                  - one arg being unlimited polymorphic, the ambiguous tb has class(*) arg
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
         procedure :: printa
         procedure :: printb

         generic :: print => printa, printb

   end type

   contains

      subroutine printa(a, b)
         class(b1(4)), intent(in) :: a
         class(*),  intent(in) :: b

      end subroutine

      subroutine printb(a, b)
         class(b1(4)), intent(in)  :: a
         class(*), intent(in) :: b

      end subroutine

end module

module binoperator

   type b11(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i
      contains
         procedure, pass :: adda
         procedure, pass :: addb
         generic :: operator(+) => adda, addb
   end type

   contains

      type(b11(4)) function adda(a, b)
         class(b11(4)), intent(in) :: a
         class(*), intent(in) :: b

         adda = b11(4)(10)

      end function

      type(b11(4)) function addb(a, b)
         class(b11(4)), intent(in) :: a
         class(*), intent(in) :: b

         addb = b11(4)(20)

      end function

end module

module assignment

   type b12(k3)    ! (4)
     integer, kind :: k3
     integer(k3)   :: i, j
      contains
         procedure, pass :: assgn1
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn1, assgn2
   end type

   contains
   
      subroutine assgn1(a, b)
         class(b12(4)), intent(out) :: a
         class(*), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(b12(4)), intent(out) :: a
         class(*), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound014d
end program
