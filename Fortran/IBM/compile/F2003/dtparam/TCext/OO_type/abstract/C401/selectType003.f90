!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Select Type Construct
!*                               - type is, class is, type-spec being abstract type with rename-list
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   end type

   type, extends(base), abstract :: child(k2)
      integer, kind :: k2
      real(k2) :: rid
   end type

end module

program selectType003
   use m, abstractbase => base, abstractchild => child

   class(abstractbase(4)), allocatable :: b1
   class(abstractchild(4,4)), pointer    :: c1(:)

   select type ( b1 )
      type is ( abstractbase(4) )
         error stop 1_4
   end select

   select type ( b1 )
      type is ( abstractbase(4) )
         error stop 2_4
      type is ( abstractchild(4,4) )
         error stop 3_4
      class default
   end select

end program
