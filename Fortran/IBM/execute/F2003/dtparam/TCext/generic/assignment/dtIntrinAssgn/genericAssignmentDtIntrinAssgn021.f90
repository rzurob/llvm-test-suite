! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn021.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - for allocatable component
!*                                    - use defined assignment if the "declared"
!*                                      type of the component has a type-bound
!*                                      defined assignment consistent with the components
!*                                        - base type has a defined assignment
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure :: btob
         procedure :: btoc
         generic :: assignment(=) => btob, btoc
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = -999
   end type

   type container(k2,n2)    ! (4,20)
      integer, kind                  :: k2
      integer, len                   :: n2
      type(base(:,k2)), allocatable  :: b1
      type(child(:,k2)), allocatable :: c1
   end type

   contains

      subroutine btob(a,b)
         class(base(*,4)), intent(out) :: a
         type(base(*,4)), intent(in) :: b

         a%i = b%i + 1

         print *, 'btob'

      end subroutine

      subroutine btoc(a,b)
         class(base(*,4)), intent(out) :: a
         type(child(*,4)), intent(in) :: b

         a%i = b%i + 1

         select type ( a )
            type is ( child(*,4) )
               a%j = b%j + 1
         end select

         print *, 'btoc'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn021
   use m

   type(container(4,20)) :: c1
   type(container(4,20)) :: c2, c3
   pointer :: c2
   allocatable :: c3
   target :: c1

   c1 = container(4,20)( base(20,4)(100), child(20,4)(200,300) )
   print *, c1%b1, c1%c1

   allocate ( c2, c3 )
   c2 = c1
   print *, c2%b1, c2%c1

   c3 = c2
   print *, c3%b1, c3%c1

   c3 = container(4,20)( base(20,4)(50), child(20,4)(51, 52) )
   print *, c3%b1, c3%c1

   c2 => c1
   c2 = c3

   print *, c1%b1, c1%c1
   print *, c2%b1, c2%c1

   c1%b1%i = -999

   c1%b1 = c2%c1%base
   print *, c1%b1, c1%c1

   c1%c1 = c3%c1
   print *, c1%b1, c1%c1

end program

