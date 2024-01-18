! GB DTP extension using:
! ftcx_dtp -qck -qnol -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn004.f
! opt variations: -qnock -ql -qdefaultpv -qdeferredlp -qreuse=none

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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - Perform generic type bound assignment for the type
!*                                    component when it is declared for the type
!*                                    where the component is an array, and elemental generic tb assignment is defined
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

   type com1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         generic :: assignment(=) => c1assgn
         procedure, pass :: c1assgn
   end type

   type com2(k2,n1)    ! (1,3)
      integer, kind             :: k2
      integer, len              :: n1
      character(kind=k2,len=n1) :: c
      contains
         generic :: assignment(=) => c2assgn
         procedure, pass :: c2assgn
   end type

   contains

      elemental subroutine c1assgn ( a, b )
         class(com1(4)), intent(out) :: a
         class(com1(4)), intent(in) :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine c2assgn ( a, b )
         class(com2(1,*)), intent(out) :: a
         class(com2(1,*)), intent(in) :: b

         a%c(1:3) = b%c(1:2) // 'x'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn004
   use m

   type base(k3,k4,n2)    ! (4,1,3)
      integer, kind     :: k3,k4
      integer, len      :: n2
      integer(k3)       :: x
      type(com1(k3))    :: c1(3)
      type(com2(k4,n2)) :: c2(2,2)
   end type

   type(base(4,1,3)) :: b1
   type(base(4,1,3)), allocatable :: b2
   type(base(4,1,3)), pointer :: b3, b4
   
   b1 = base(4,1,3) ( 100, (/ com1(4)(10), com1(4)(20), com1(4)(30)  /), reshape ( source = (/ com2(1,3)('abc'), com2(1,3)('def'), com2(1,3)('ghi'), com2(1,3)('jkl') /), shape =(/ 2, 2 /) ) )
   print *, b1
   
   allocate ( b2 )
   
   b2 = b1
   print *, b2
   
   allocate ( b4 )
   b3 => b4
   
   b3 = b2
   
   print *, b3
   print *, b4

end program
