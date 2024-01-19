! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentFinal001.f
! opt variations: -qnol -qdeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: Finalization for generic assignment tb
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
      integer, kind        :: k1
      integer, len         :: n1
      integer(k1), pointer :: i
      contains
         generic :: assignment(=) => bassgn
         procedure, pass :: bassgn
         final :: bfinal
   end type

   contains

   subroutine bassgn ( a, b )
      class(base(*,4)), intent(out) :: a
      type(base(*,4)), intent(in)   :: b

       a%i = b%i

       print *, 'bassign'

   end subroutine

   subroutine bfinal ( a )
      type(base(*,4)), intent(inout) :: a

      write (*,'(1x,a)',advance='no') 'bfinal'

      if ( associated ( a%i ) ) then
         write (*,'(":",i6)',advance='no') a%i
       	 nullify ( a%i )
      end if

      allocate ( integer :: a%i )
      write (*,'(\)')

   end subroutine

end module

program genericAssignmentFinal001
   use m

   type(base(20,4)) :: b1
   type(base(20,4)), allocatable :: b2
   type(base(20,4)), pointer :: b3

   integer, target :: i1 = 1227

   allocate ( b2, b3 )

   b1 = base(20,4)(i1)
   print *, b1%i

   b2 = b1
   print *, b2%i

   b3 = b1
   print *, b3%i

end program
