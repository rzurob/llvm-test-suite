! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentDeferred001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: with deferred binding
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

   type, abstract :: incompletept(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: x = -999
      contains
         procedure(dintf), deferred, pass :: assgn
         generic :: assignment(=) => assgn
   end type

   type, extends(incompletept) :: pt    ! (20,4)
      integer(k1) :: y = -999
      contains
         procedure, pass :: assgn
   end type

   interface
      subroutine dintf (a, b)
         import incompletept
         class(incompletept(*,4)), intent(out) :: a
         class(incompletept(*,4)), intent(in) :: b
      end subroutine
   end interface

   contains

   subroutine assgn ( a, b)
      class(pt(*,4)), intent(out) :: a
      class(incompletept(*,4)), intent(in) :: b

      a%x = b%x
      select type ( b )
         type is ( pt(*,4) )
            a%y = b%y
      end select

      print *, 'assgn'

   end subroutine

end module

program genericAssignmentDeferred001
   use m

   class(incompletept(:,4)), pointer :: p1
   type(pt(20,4)), target :: p2

   class(pt(20,4)), allocatable :: p3

   allocate ( p1, source = pt(20,4)(3,6) )
   p2 = pt(20,4) ( 4, 8 )

   allocate ( p3 )

   p1 = p2
   p3 = p1

   select type ( p1 )
      type is ( pt(*,4) )
         print *, p1
   end select

   print *, p2

   select type ( p3 )
      type is ( pt(*,4) )
         print *, p3
   end select

   p1 = pt(20,4) ( 10, 20 )

   select type ( p1 )
      type is ( pt(*,4) )
         print *, p1
   end select

end program
