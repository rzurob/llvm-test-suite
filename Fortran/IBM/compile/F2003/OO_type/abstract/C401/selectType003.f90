!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
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

   type, abstract :: base
      integer :: id
   end type

   type, extends(base), abstract :: child
      real :: rid
   end type

end module

program selectType003
   use m, abstractbase => base, abstractchild => child

   class(abstractbase), allocatable :: b1
   class(abstractchild), pointer    :: c1(:)

   select type ( b1 )
      type is ( abstractbase )
         error stop 1_4
   end select
   
   select type ( b1 )
      type is ( abstractbase )
         error stop 2_4
      type is ( abstractchild )
         error stop 3_4
      class default      
   end select

end program
