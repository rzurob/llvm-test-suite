!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - Elemental Deferred Binding in base type
!*                                    call the elemental type bound with dummy-arg, associate-name
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
         procedure(inf), deferred, pass :: setid
   end type

   interface
      elemental subroutine inf(dtv,j)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   type, extends(base) :: child
      integer :: j
      contains
         procedure, pass :: setid
   end type

   contains

      elemental subroutine setid(dtv,j)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: j

         dtv%i = j
         dtv%j = j

      end subroutine

end module

program deferred003
   use n

   class(base), pointer     :: b1(:,:)
   class(base), allocatable :: b2(:,:)
   class(child), pointer :: c1(:)

   allocate ( b1(2,2), source = reshape ( source = (/ child(101,1001), child(101, 1001), child(101, 1001), child(101, 1001) /), shape =(/2,2/) ) )
   allocate ( b2(2,2), source = b1 )

   allocate ( c1(3), source = (/ ( child (101, 1001), i=1,3) /) )


   associate ( g => b1(1:2,1) )
      call g%setid(10)
   end associate

   call foo(b2)

   select type ( gg => c1(1:3:2) )
      class default
         call gg%setid(30)
   end select


   select type ( b1 )
      type is ( child )
         if ( ( b1(1,1)%i /= 10 )  .or. ( b1(1,1)%j /= 10 ) .or. &
              ( b1(2,1)%i /= 10 )  .or. ( b1(2,1)%j /= 10 ) .or. &
              ( b1(1,2)%i /= 101 ) .or. ( b1(1,2)%j /= 1001 )    .or. &
              ( b1(2,2)%i /= 101 ) .or. ( b1(2,2)%j /= 1001 ) )  error stop 1_4
      class default
         error stop 2_4
   end select

   select type ( b2 )
      type is ( child )
         if ( ( b2(1,1)%i /= 20 )  .or. ( b2(1,1)%j /= 20 )   .or. &
              ( b2(2,1)%i /= 101 ) .or. ( b2(2,1)%j /= 1001 ) .or. &
              ( b2(1,2)%i /= 20 )  .or. ( b2(1,2)%j /= 20 )   .or. &
              ( b2(2,2)%i /= 101 ) .or. ( b2(2,2)%j /= 1001 ) )  error stop 2_4
      class default
         error stop 2_4
   end select

   if ( ( c1(1)%i /= 30 )  .or. ( c1(1)%j /= 30 )      .or. &
        ( c1(2)%i /= 101 ) .or. ( c1(2)%j /= 1001 )    .or. &
        ( c1(3)%i /= 30 )  .or. ( c1(3)%j /= 30 ) )    error stop 3_4

   contains

      subroutine foo(dtv)
         class(base), intent(inout) :: dtv(:,:)
         call dtv(1,1:2)%setid(20)
      end subroutine

end program
