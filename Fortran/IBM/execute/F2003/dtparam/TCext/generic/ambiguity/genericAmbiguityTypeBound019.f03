! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound019.f
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
!*                                     ( Base )
!*                                      /    \
!*                                 (Child1) (Child2)
!*
!*                                 for generic-name, define ambiguous tb in two child types, which is legal
!*                                 for operator and assignment, define non-ambiguous tb in two child types
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
   end type

   type, extends(b1) :: c1    ! (20,4)
      contains
         procedure, nopass :: twoargs1
         generic :: print => twoargs1
   end type

   type, extends(b1) :: c2    ! (20,4)
      contains
         procedure, nopass :: twoargs2
         generic :: print => twoargs2
   end type

   contains

      subroutine twoargs1(a, b)
         class(c1(*,4)), intent(in) :: a
         class(b1(*,4)), intent(in) :: b

         print *, 'twoargs1'

      end subroutine

      subroutine twoargs2(a, b)
         class(b1(*,4)), intent(in) :: a
         class(c2(*,4)), intent(in) :: b

         print *, 'twoargs2'

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
         procedure, pass(b) :: addb
         generic :: operator(+) => addb
   end type

   type, extends(b11) :: c12    ! (20,4)
   end type

   contains

      type(b11(20,4)) function adda(a, b)
         class(b11(*,4)), intent(in) :: a
         class(c12(*,4)), intent(in) :: b

         adda = b11(20,4)(10)

         print *, 'adda'

      end function

      type(b11(20,4)) function addb(a, b)
         class(b11(*,4)), intent(in) :: a
         class(c11(*,4)), intent(in) :: b

         addb = b11(20,4)(10)

         print *, 'addb'

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
         procedure, pass(a) :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(b21) :: c22    ! (20,4)
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(c21(*,4)), intent(out) :: a
         class(b21(*,4)), intent(in)  :: b

         print *, 'assgn1'

      end subroutine

      subroutine assgn2(a, b)
         class(c22(*,4)), intent(out) :: a
         class(b21(*,4)), intent(in)  :: b

         print *, 'assgn2'

      end subroutine

end module

program genericAmbiguityTypeBound019
   use genericName
   use binoperator
   use assignment

   type(b1(20,4)) :: b1_1
   type(c1(20,4)) :: c1_1
   type(c2(20,4)) :: c2_1

   type(b11(20,4)) :: b11_1
   type(c11(20,4)) :: c11_1
   type(c12(20,4)) :: c12_1

   type(b21(20,4)) :: b21_1
   type(c21(20,4)) :: c21_1
   type(c22(20,4)) :: c22_1

   ! generic name

   call c1_1%print(c1_1, c1_1)
   call c1_1%print(c1_1, b1_1)
   call c1_1%print(c1_1, c2_1)

   call c2_1%print(c1_1, c2_1)
   call c2_1%print(b1_1, c2_1)

   ! binoperator

   b11_1 = b11_1 + c12_1
   b11_1 = b11_1 + c11_1

   b11_1 = c11_1 + c12_1
   b11_1 = c12_1 + c11_1

   ! assignment

   c21_1 = b21_1
   c22_1 = b21_1

   c21_1 = c22_1
   c22_1 = c21_1

end program
