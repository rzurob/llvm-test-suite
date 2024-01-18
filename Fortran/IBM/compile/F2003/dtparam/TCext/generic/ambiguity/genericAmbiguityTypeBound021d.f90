! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound021d.f
! opt variations: -qnol

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
!*  DESCRIPTION                : With Class Hierarchy
!*
!*                                               ( Base )
!*                                      /       /         \        \
!*                                 (Child1) (Child2)    (Child3) (Child4)
!*
!*                               - with more child types
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

         generic :: twoarga => twoargs1, twoargs2, twoargs3, twoargs4

         procedure, nopass :: twoargs1
         procedure, nopass :: twoargs2
         procedure, nopass :: twoargs3
         procedure, nopass :: twoargs4
   end type

   type, extends(b1) :: c1    ! (20,4)
   end type

   type, extends(b1) :: c2    ! (20,4)
   end type

   type, extends(b1) :: c3    ! (20,4)
   end type

   type, extends(b1) :: c4    ! (20,4)
   end type

   contains

      subroutine twoargs1(a, b)
         class(b1(*,4)), intent(in) :: a
         class(c1(*,4)), intent(in) :: b

      end subroutine

      subroutine twoargs2(a, b)
         class(c1(*,4)), intent(in) :: a
         class(c2(*,4)), intent(in) :: b

      end subroutine

      subroutine twoargs3(a, b)
         class(c3(*,4)), intent(in) :: a
         class(c4(*,4)), intent(in) :: b

      end subroutine

      subroutine twoargs4(a, b)
         class(c4(*,4)), intent(in) :: a
         class(c1(*,4)), intent(in) :: b

      end subroutine

end module

module binoperator

   type b11(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i
      contains
         procedure, pass :: adda
         generic :: operator(+) => adda
   end type

   type, extends(b11) :: c11    ! (20,4)
      contains
         procedure, pass :: addb
         generic :: operator(+) => addb
   end type

   type, extends(b11) :: c12    ! (20,4)
      contains
         procedure, pass :: addc
         generic :: operator(+) => addc
   end type

   type, extends(b11) :: c13    ! (20,4)
      contains
         procedure, pass :: addd
         generic :: operator(+) => addd
   end type

   type, extends(b11) :: c14    ! (20,4)
      contains
         procedure, pass :: adde
         generic :: operator(+) => adde
   end type

   contains

      type(b11(20,4)) function adda(a, b)
         class(b11(*,4)), intent(in) :: a
         class(c11(*,4)), intent(in) :: b

         adda = b11(20,4)(10)

      end function

      type(b11(20,4)) function addb(a, b)
         class(c11(*,4)), intent(in) :: a
         class(c12(*,4)), intent(in) :: b

         addb = b11(20,4)(10)

      end function

      type(b11(20,4)) function addc(a, b)
         class(c12(*,4)), intent(in) :: a
         class(c13(*,4)), intent(in) :: b

         addc = b11(20,4)(10)

      end function

      type(b11(20,4)) function addd(a, b)
         class(c13(*,4)), intent(in) :: a
         class(c11(*,4)), intent(in) :: b

         addd = b11(20,4)(10)

      end function

      type(b11(20,4)) function adde(a, b)
         class(c14(*,4)), intent(in) :: a
         class(c12(*,4)), intent(in) :: b

         adde = b11(20,4)(10)

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
         procedure, pass(a) :: assgn2
         generic :: assignment(=) => assgn2
   end type

   type, extends(b21) :: c23    ! (20,4)
      contains
         procedure, pass(a) :: assgn3
         generic :: assignment(=) => assgn3
   end type

   type, extends(b21) :: c24    ! (20,4)
      contains
         procedure, pass(b) :: assgn4
         generic :: assignment(=) => assgn4
   end type

   contains

      subroutine assgn1(a, b)
         class(c21(*,4)), intent(out) :: a
         class(c21(*,4)), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(c22(*,4)), intent(out) :: a
         class(c22(*,4)), intent(in)  :: b

      end subroutine

      subroutine assgn3(a, b)
         class(c23(*,4)), intent(out) :: a
         class(c24(*,4)), intent(in)  :: b

      end subroutine

      subroutine assgn4(a, b)
         class(b21(*,4)), intent(out) :: a
         class(c24(*,4)), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound021d
end program
