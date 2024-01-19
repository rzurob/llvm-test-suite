!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - non-pure Deferred Binding in base type
!*                                    non-pure and pure binding in descendent types
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
      integer :: i
      contains
         procedure(inf), deferred, pass :: print
   end type

   interface
      subroutine inf(dtv)
         import base
         class(base), intent(inout) :: dtv
      end subroutine
   end interface

   type, extends(base) :: child
      integer :: j
      contains
         procedure, pass :: print => printchild
   end type

   type, extends(child) :: gen3
      integer :: k
      contains
         procedure, pass :: print => cannotprintgen3
   end type

   interface
      pure subroutine cannotprintgen3(dtv)
         import gen3
         class(gen3), intent(inout) :: dtv
      end subroutine
   end interface

   contains

      subroutine printchild(dtv)
         class(child), intent(inout) :: dtv
         print *, dtv%i, dtv%j
      end subroutine

end module

pure subroutine cannotprintgen3(dtv)
   use n, only: gen3
   class(gen3), intent(inout) :: dtv
   dtv%i = 123
   dtv%j = 234
   dtv%k = 345
end subroutine

program deferred004
   use n

   class(base), pointer :: b1
   class(child), allocatable, target :: c1

   allocate ( b1, source = child ( 101, 102 ) )
   allocate ( c1, source = child ( 103, 104 ) )

   call b1%print()
   call c1%print()

   deallocate ( b1, c1 )

   allocate ( c1, source = gen3( 201, 202, 203 ) )

   b1 => c1

   select type ( b1 )
      type is ( gen3 )
         select type ( c1 )
            type is ( gen3 )
               call b1%child%print()
               call c1%child%print()
               if ( ( b1%k /= 203 ) .or. ( c1%k /= 203 ) ) error stop 1_4
         end select
   end select

   call b1%print()

   select type ( b1 )
      type is ( gen3 )
         select type ( c1 )
            type is ( gen3 )
               call b1%child%print()
               call c1%child%print()
               if ( ( b1%k /= 345 ) .or. ( c1%k /= 345 ) ) error stop 1_4
         end select
   end select

end program
