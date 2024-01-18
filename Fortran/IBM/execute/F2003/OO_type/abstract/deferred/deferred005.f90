!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - Recursive Deferred Binding in base type
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

module n

   type, abstract :: base
      class(base), pointer :: next => null()
      integer :: i
      contains
         procedure(inf), deferred, pass :: print
   end type

   interface
      recursive subroutine inf(dtv)
         import base
         class(base), intent(inout) :: dtv
      end subroutine
   end interface

end module


module n1
   use n, only: base

   type, extends(base) :: child
      integer :: j
      contains
         procedure, pass :: print => printchild
   end type

   type, extends(child) :: gen3
      integer :: k
      contains
         procedure, pass :: print => printgen3
   end type

   contains

      recursive subroutine printchild(dtv)
         class(child), intent(inout) :: dtv
         print *, dtv%i, dtv%j
         if ( associated ( dtv%next ) ) then
            call dtv%next%print()
         end if
      end subroutine

      recursive subroutine printgen3(dtv)
         class(gen3), intent(inout) :: dtv
         print *, dtv%i, dtv%j, dtv%k
         if ( associated ( dtv%next ) ) then
            call dtv%next%print()
         end if
      end subroutine

end module

program deferred005
   use n1

   class(base), pointer :: b1
   class(child), allocatable :: c1

   allocate ( b1, source = child( i=1, j=11 ) )
   allocate ( b1%next, source = gen3( i=2, j=22, k=222 ) )
   allocate ( b1%next%next, source = child( i=3, j=33 ) )
   allocate ( b1%next%next%next, source = gen3( i=4, j=44, k=444 ) )
   allocate ( b1%next%next%next%next, source = child( i=5, j=55 ) )
   allocate ( b1%next%next%next%next%next, source = gen3( i=6, j=66, k=666 ) )

   call b1%print()

   allocate ( c1, source = child( i = -1, j = -11 ) )
   allocate ( c1%next, source = b1 )

   call c1%print()

end program
