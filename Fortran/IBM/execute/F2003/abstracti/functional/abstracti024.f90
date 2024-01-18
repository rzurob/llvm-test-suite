!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - Recursive Deferred Binding in base and child type
!*                                    and non-recursive in gen3 type
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

   abstract interface
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

      subroutine printgen3(dtv)
         class(gen3), intent(inout) :: dtv
         print *, dtv%i, dtv%j, dtv%k
      end subroutine

end module

program abstracti024
   use n1

   class(base), pointer :: b1
   class(child), allocatable :: c1

   allocate ( b1, source = child( i=1, j=11 ) )
   allocate ( b1%next, source = gen3( i=2, j=22, k=222 ) )
   allocate ( b1%next%next, source = child( i=3, j=33 ) )
   allocate ( b1%next%next%next, source = child( i=4, j=44 ) )
   allocate ( b1%next%next%next%next, source = child( i=5, j=55 ) )
   allocate ( b1%next%next%next%next%next, source = child( i=6, j=66 ) )

   call b1%print()
   call b1%next%next%print()

   allocate ( c1, source = child( i = -1, j = -11 ) )
   c1%next => b1%next%next

   call c1%print()

end program abstracti024
