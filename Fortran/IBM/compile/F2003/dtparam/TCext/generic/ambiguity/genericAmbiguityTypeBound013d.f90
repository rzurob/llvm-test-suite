! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound013d.f
! opt variations: -qnok -qnol -qreuse=none

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
!*  DESCRIPTION                : two argument with pass-arg to be first/second arg specified (for generic-name, operator, and assignment tb)
!*                                  - one different independent types, but ambiguous type bound
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

   type b2(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i
   end type
   
   type, extends(b2) :: c2    ! (20,4)
      integer(k2) :: j
   end type

   interface print
      module procedure printa
      module procedure printb
   end interface

   contains

      subroutine printa(a, b)
         class(b1(*,4)), intent(in) :: a
         class(b2(*,4)),  intent(in) :: b

      end subroutine

      subroutine printb(a, b)
         type(b1(*,4)), intent(in)  :: a
         type(c2(*,4)), intent(in) :: b

      end subroutine

end module

module binoperator

   type b11(n3,k3)    ! (20,4)
      integer, kind :: k3
      integer, len  :: n3
      integer(k3)   :: i
      contains
         procedure, pass :: adda
         generic :: operator(+) => adda
   end type

   type b12(n4,k4)    ! (20,4)
      integer, kind :: k4
      integer, len  :: n4
      integer(k4)   :: i
   end type

   type, extends(b12) :: c12    ! (20,4)
      contains
         procedure, pass(b) :: addb
         generic :: operator(+) => addb
   end type
   contains

      type(b11(20,4)) function adda(a, b)
         class(b11(*,4)), intent(in) :: a
         class(b12(*,4)), intent(in) :: b

         adda = b11(20,4)(10)

      end function

      type(b11(20,4)) function addb(a, b)
         class(b11(*,4)), intent(in) :: a
         class(c12(*,4)), intent(in) :: b

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
         
   end type
   
   type, extends(b12) :: c12    ! (20,4)
      contains
         generic :: assignment(=) => assgn1      
   end type

   type b22(k6,n6)    ! (4,20)
       integer, kind :: k6
       integer, len  :: n6
      contains
         procedure, pass(b) :: assgn2
         generic :: assignment(=) => assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(b12(*,4)), intent(out) :: a
         type(b22(4,*)), intent(in)   :: b

      end subroutine

      subroutine assgn2(a, b)
         class(b12(*,4)), intent(out) :: a
         class(b22(4,*)), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound013d
end program
