! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn002.f
! opt variations: -qck -qnok -ql -qdefaultpv -qdeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - type component has a suitable generic assignment interface
!*                                    however derived type intrinsic assignment should not use it.
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
   end type

   type com2(k2,n1)    ! (4,3)
      integer, kind :: k2
      integer, len  :: n1
      character(n1) :: c
   end type

   type base(k3,n2)    ! (4,3)
      integer, kind     :: k3
      integer, len      :: n2
      integer(k3)       :: x
      type(com1(k3))    :: c1
      type(com2(k3,n2)) :: c2
   end type

   interface assignment(=)
      module procedure c1assgn
      module procedure c2assgn
   end interface

   contains

      subroutine c1assgn ( a, b )
         class(com1(4)), intent(out) :: a
         class(com1(4)), intent(in) :: b

         a%i = b%i
         print *, 'c1assgn'

      end subroutine

      subroutine c2assgn ( a, b )
         class(com2(4,*)), intent(out) :: a
         class(com2(4,*)), intent(in) :: b

         a%c = b%c
         print *, 'c2assgn'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn002
   use m

   type(base(4,3)) :: b1
   type(base(4,3)), allocatable :: b2
   type(base(4,3)), pointer :: b3

   type(com2(4,3)), parameter :: c2 = com2(4,3)('ftn')

   b1 = base(4,3)( 10, com1(4)(20), com2(4,3)('ibm') )
   print *, b1

   allocate ( b2, b3 )

   b2 = base(4,3)( 30, com1(4)(40), c2 )
   print *, b2

   b3 = b1
   print *, b3

   b2 = b3
   print *, b2

end program
