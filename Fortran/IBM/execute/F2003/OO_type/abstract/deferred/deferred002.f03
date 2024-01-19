!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - Elemental Private Deferred Binding in base type, use assumed-shape array
!*                                    as a whole array calling the elemetnal type bound
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
         procedure(inf), private, deferred, pass :: setid
   end type

   interface
      elemental subroutine inf(dtv,j)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   type, extends(base) :: child
      character(3) :: c
      contains
         procedure, pass :: setid
   end type

   contains

      subroutine setidwrapper ( dtv, i )
         class(base), intent(inout) :: dtv(:)
         integer :: i

         call dtv%setid(i)

      end subroutine

      elemental subroutine setid(dtv,j)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: j

         dtv%i = j

      end subroutine

end module

program deferred002
   use n

   class(base), allocatable, target :: b1(:)
   class(base), pointer     :: b2(:)

   allocate ( b1(3), source = (/child(101,'abc'),child(101,'def'),child(101,'ghi') /) )
   b2 => b1

   call setidwrapper( b1, 1000 )

   select type ( b1 )
      type is ( child )
         select type ( b2 )
            type is ( child )
               if ( ( b1(1)%i /= 1000 ) .or. ( b1(2)%i /= 1000 ) .or. ( b1(3)%i /= 1000 ) .or. &
                  ( b2(1)%i /= 1000 ) .or. ( b2(2)%i /= 1000 ) .or. ( b2(3)%i /= 1000 ) .or. &
                  ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) .or. &
                  ( b2(1)%c /= 'abc' ) .or. ( b2(2)%c /= 'def' ) .or. ( b2(3)%c /= 'ghi' ) ) error stop 1_4
         end select
   end select

   call setidwrapper ( b2, 10 )

   select type ( b1 )
      type is ( child )
         select type ( b2 )
            type is ( child )
               if ( ( b1(1)%i /= 10 ) .or. ( b1(2)%i /= 10 ) .or. ( b1(3)%i /= 10 ) .or. &
                  ( b2(1)%i /= 10 ) .or. ( b2(2)%i /= 10 ) .or. ( b2(3)%i /= 10 ) .or. &
                  ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) .or. &
                  ( b2(1)%c /= 'abc' ) .or. ( b2(2)%c /= 'def' ) .or. ( b2(3)%c /= 'ghi' ) ) error stop 1_4
         end select
   end select


end program
