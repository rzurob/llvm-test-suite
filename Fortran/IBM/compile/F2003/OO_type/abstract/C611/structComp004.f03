!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        non-polymorphic abstract type data-ref used in intrinsic function
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

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
      real :: rid
   end type

end module

program structComp004
   use m

   class(child), pointer    :: c1
   class(base), allocatable :: b1
   class(*), pointer        :: u1

   allocate ( b1, source = child( 3, 4.0 ) )
   allocate ( c1, source = child( 1, 2.0 ) )

   select type ( b1 )
      type is ( child )
         if ( same_type_as(b1%base,c1%base) ) error stop 1_4
         if ( extends_type_of(b1%base,b1) )   error stop 2_4
         if ( extends_type_of(b1%base,u1) )   error stop 3_4
   end select

end program