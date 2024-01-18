! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound023d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : With Class Hierarchy
!*
!*                                      ( Base )
!*                                      /        \
!*                                 (Child1)   (Child2)
!*                                     |         |
!*                                 (Gen31)    (Gen32)
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

   type b1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, nopass :: twoargs1
         procedure, nopass :: twoargs2
         generic :: twoargs => twoargs1, twoargs2
   end type

   type, extends(b1) :: c1    ! (20,4)
   end type

   type, extends(b1) :: c2    ! (20,4)
   end type

   type, extends(c1) :: g1    ! (20,4)
   end type

   type, extends(c2) :: g2    ! (20,4)
      contains
         procedure, nopass :: twoargs3
         generic :: twoargs => twoargs3
   end type

   contains

      subroutine twoargs1(a, b)
         class(b1(*,4)), intent(in) :: a
         type(b1(*,4)), intent(in) :: b

      end subroutine

      subroutine twoargs2(a, b)
         class(b1(*,4)), intent(in) :: a
         class(c1(*,4)), intent(in) :: b

      end subroutine

      subroutine twoargs3(a, b)
         type(c1(*,4)), intent(in) :: a
         class(b1(*,4)), intent(in) :: b

      end subroutine

end module

module unoperator

   type b11(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i
      contains
         procedure, pass :: addb
         generic :: operator(.myun.) => addb
   end type

   type, extends(b11) :: c11    ! (20,4)
   end type

   type, extends(b11) :: c12    ! (20,4)
   end type

   type, extends(c11) :: g11    ! (20,4)
   end type

   type, extends(c12) :: g12    ! (20,4)
      contains
         procedure, pass :: addc
         generic :: operator(.myun.) => addc
   end type

   contains

      type(b11(20,4)) function addb(a)
         class(b11(*,4)), intent(in) :: a

         addb = b11(20,4)(10)

      end function

      type(b11(20,4)) function addc(b)
         class(g12(*,4)), intent(in) :: b

         addc = b11(20,4)(10)

      end function

end module

module assignment

   type b21(n3,k3)    ! (20,4)
      integer, kind :: k3
      integer, len  :: n3
      integer(k3)   :: i
   end type

   type, extends(b21) :: c21    ! (20,4)
      contains
         procedure, pass(b) :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(b21) :: c22    ! (20,4)
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   type, extends(c21) :: g21    ! (20,4)

   end type

   type, extends(c22) :: g22    ! (20,4)
   end type

   contains

      subroutine assgn1(a, b)
         type(g22(*,4)), intent(out) :: a
         class(c21(*,4)), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(c22(*,4)), intent(out) :: a
         type(c21(*,4)), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound023d
end program
