! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qnodeferredlp -qreuse=none /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn006.f
! opt variations: -qnol -qdefaultpv -qdeferredlp -qreuse=self -qreuse=base

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - Perform generic type bound assignment for the type
!*                                    component when it is declared for the type
!*                                    when components that has type hierarchy
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

   type com(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         generic :: assignment(=) => c1assgn
         procedure, pass :: c1assgn
   end type

   type, extends(com) :: comchild(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j
   end type

   contains

      subroutine c1assgn ( a, b )
         class(com(*,4)), intent(out) :: a
         class(com(*,4)), intent(in) :: b

         a%i = b%i
         print *, 'c1assgn'

         select type ( a )
            type is ( comchild(*,4,*,4) )
               select type ( b )
                  type is ( comchild(*,4,*,4) )
                     print *, 'child'
                     a%j = b%j
               end select
         end select

      end subroutine

end module

module m1
   use m

   type base(n3,k3)    ! (20,4)
      integer, kind                        :: k3
      integer, len                         :: n3
      integer(k3)                          :: x
      type(com(n3,k3))                     :: c1
      type(com(n3,k3)), pointer            :: c2
      type(comchild(n3,k3,n3,k3))          :: cc1
      type(comchild(n3,k3,n3,k3)), pointer :: cc2
   end type

end module

program genericAssignmentDtIntrinAssgn006
   use m1

   type(base(20,4)) :: b1
   type(base(20,4)), pointer :: b2
   type(base(20,4)), allocatable :: b3

   type(com(20,4)), target :: c1 = com(20,4)(200)
   type(comchild(20,4,20,4)), target :: cc1 = comchild(20,4,20,4)(500,600)

   b1 = base(20,4) ( 100, com(20,4)(100), c1, comchild(20,4,20,4)(300,400), null() )
   print *, b1%x, b1%c1, b1%c2, b1%cc1, associated (b1%cc2)

   allocate ( b2, b3 )
   b2 = b1
   print *, b2%x, b2%c1, b2%c2, b2%cc1, associated (b2%cc2)

   b2%cc2 => cc1
   print *, associated (b2%cc2), b2%cc2

   b3 = b2
   print *, b3%x, b3%c1, b3%c2, b3%cc1, b3%cc2, associated ( b3%c2 , b2%c2 ), associated ( b3%cc2 , b2%cc2 )

end program
