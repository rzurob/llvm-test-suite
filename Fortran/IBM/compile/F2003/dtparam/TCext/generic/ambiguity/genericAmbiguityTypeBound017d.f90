! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound017d.f
! opt variations: -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : two argument with pass-arg to be first arg specified (for generic-name, operator, and assignment tb)
!*                                  - one arg being unlimited polymorphic, the ambiguous tb has another derived type arg
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
         procedure :: printa
         procedure :: printb

         generic :: print => printa, printb

   end type

   type dt(k2,n2)    ! (4,20)
       integer, kind :: k2
       integer, len  :: n2
   end type


   contains

      subroutine printa(a, b)
         class(b1(*,4)), intent(in) :: a
         class(*),  intent(in) :: b

      end subroutine

      subroutine printb(a, b)
         class(b1(*,4)), intent(in)  :: a
         class(dt(4,*)), intent(in) :: b

      end subroutine

end module

module binoperator

   type b11(n3,k3)    ! (20,4)
      integer, kind :: k3
      integer, len  :: n3
      integer(k3)   :: i
      contains
         procedure, pass :: adda
         procedure, pass :: addb
         generic :: operator(+) => adda, addb
   end type

   type st(n4,k4)    ! (20,4)
      integer, kind :: k4
      integer, len  :: n4
      sequence
      integer(k4)   :: j
   end type

   contains

      type(b11(20,4)) function adda(a, b)
         class(b11(*,4)), intent(in) :: a
         class(*), intent(in) :: b

         adda = b11(20,4)(10)

      end function

      type(b11(20,4)) function addb(a, b)
         class(b11(*,4)), intent(in) :: a
         type(st(*,4)), intent(in) :: b

         addb = b11(20,4)(20)

      end function

end module

module assignment

   type b12(n5,k5)    ! (20,4)
     integer, kind :: k5
     integer, len  :: n5
     integer(k5)   :: i, j
      contains
         procedure, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(b12) :: c12    ! (20,4)
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   type, bind(c) :: bct
   end type

   contains

      subroutine assgn1(a, b)
         class(b12(*,4)), intent(out) :: a
         class(*), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(c12(*,4)), intent(out) :: a
         type(bct), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound017d
end program
