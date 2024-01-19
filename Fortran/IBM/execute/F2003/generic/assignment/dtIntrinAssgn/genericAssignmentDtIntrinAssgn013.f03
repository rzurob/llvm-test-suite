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

   type inner
      integer, pointer :: i => null()
      contains
         procedure, pass(secondarg) :: itoi
         generic :: assignment(=) => itoi
   end type

   type container
      type(inner) :: inn
      character(3) :: c
   end type

   contains

      subroutine itoi( firstarg, secondarg )
         class(inner), intent(out) :: firstarg
         class(inner), intent(in) :: secondarg
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

   type(container) :: c1, c2
   allocatable :: c1

   integer, target :: i1  = 100_4

   allocate ( c1 )

   c1 = container(inner(), 'ibm')
   print *, c1%inn%i, c1%c

   c1%inn%i => i1

   c2 = c1
   print *, c2%inn%i, c2%c, associated ( c2%inn%i, c1%inn%i ), associated ( c2%inn%i, i1 ), associated ( c1%inn%i, i1 )

   c2 = container(inner(null()), 'ftn')
   print *, c2%inn%i, c2%c

end program
