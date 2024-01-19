! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound018d.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : With Class Hierarchy
!*
!*                                     ( Base )
!*                                      /    \
!*                                 (Child1) (Child2)
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
         procedure, nopass :: twoargs1
         procedure, nopass :: twoargs2

         generic :: print => twoargs1, twoargs2

   end type

   type, extends(b1) :: c1    ! (4)
   end type

   type, extends(b1) :: c2    ! (4)
   end type

   contains

      subroutine twoargs1(a, b)
         class(c1(4)), intent(in) :: a
         class(b1(4)), intent(in) :: b

      end subroutine

      subroutine twoargs2(a, b)
         class(b1(4)), intent(in) :: a
         class(c2(4)), intent(in) :: b

      end subroutine

end module

module binoperator

   type b11(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i
      contains
         procedure, pass :: adda
         generic :: operator(+) => adda
   end type

   type, extends(b11) :: c11    ! (4)
      contains
         procedure, pass :: addb
         generic :: operator(+) => addb
   end type

   type, extends(b11) :: c12    ! (4)
   end type

   contains

      type(b11(4)) function adda(a, b)
         class(b11(4)), intent(in) :: a
         class(c12(4)), intent(in) :: b

         adda = b11(4)(10)

      end function

      type(b11(4)) function addb(a, b)
         class(c11(4)), intent(in) :: a
         class(b11(4)), intent(in) :: b

         addb = b11(4)(10)

      end function

end module

module assignment

   type b21(k3)    ! (4)
      integer, kind :: k3
      integer(k3)   :: i
   end type

   type, extends(b21) :: c21    ! (4)
      contains
         procedure, pass(a) :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(b21) :: c22    ! (4)
      contains
         procedure, pass(b) :: assgn2
         generic :: assignment(=) => assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(c21(4)), intent(out) :: a
         class(b21(4)), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(b21(4)), intent(out) :: a
         class(c22(4)), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound018d
end program
