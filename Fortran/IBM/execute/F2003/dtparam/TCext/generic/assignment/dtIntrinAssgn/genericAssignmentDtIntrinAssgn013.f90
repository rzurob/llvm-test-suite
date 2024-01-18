! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn013.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                      - container type contains a derived type component that has UD assignment with pass on second arg
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

   type inner(n1,k1)    ! (20,4)
      integer, kind        :: k1
      integer, len         :: n1
      integer(k1), pointer :: i => null()
      contains
         procedure, pass(secondarg) :: itoi
         generic :: assignment(=) => itoi
   end type

   type container(k2,n2,n3)    ! (4,3,20)
      integer, kind      :: k2
      integer, len       :: n2,n3
      type(inner(n3,k2)) :: inn
      character(n2)      :: c
   end type

   contains

      subroutine itoi( firstarg, secondarg )
         class(inner(*,4)), intent(out) :: firstarg
         class(inner(*,4)), intent(in) :: secondarg
         if ( .not. associated ( firstarg%i ) ) allocate ( firstarg%i )

         if ( associated ( secondarg%i ) ) then
            firstarg%i = secondarg%i
         else
            firstarg%i = -999
         end if
         print *, 'itoi'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn013
   use m

   type(container(4,3,20)) :: c1, c2
   allocatable :: c1

   integer, target :: i1  = 100_4

   allocate ( c1 )

   c1 = container(4,3,20)(inner(20,4)(), 'ibm')
   print *, c1%inn%i, c1%c

   c1%inn%i => i1

   c2 = c1
   print *, c2%inn%i, c2%c, associated ( c2%inn%i, c1%inn%i ), associated ( c2%inn%i, i1 ), associated ( c1%inn%i, i1 )

   c2 = container(4,3,20)(inner(20,4)(null()), 'ftn')
   print *, c2%inn%i, c2%c

end program
