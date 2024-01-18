!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - for allocatable component
!*                                    - if component of expr is allocated, component
!*                                      of variable is allocated with the same dynamic type
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

   type base
      integer :: i = -999
   end type base

   type, extends(base) :: child
      integer :: j = -999
   end type

   type, extends(child) :: gen3
      integer :: k = -999
   end type

   type container
      class(base), allocatable :: inn0
   end type

end module

program genericAssignmentDtIntrinAssgn018
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   c1 = container(base(100))

   allocate ( c2, c3 )

   if ( .not. same_type_as(c1%inn0,base() ) ) error stop 1_4

   c2 = c1
   c3 = c2

   if ( ( .not. same_type_as(c2%inn0,base()) ) .or. ( c2%inn0%i /= 100 ) ) error stop 2_4
   if ( ( .not. same_type_as(c3%inn0,base()) ) .or. ( c3%inn0%i /= 100 ) ) error stop 3_4

   c2 = container(child(200,300))

   c1 = c2
   c3 = c2

   select type ( g => c1%inn0 )
      type is ( child )
         if ( ( g%i /= 200 ) .or. ( g%j /= 300 ) ) error stop 4_4
      class default
         error stop 5_4
   end select

   select type ( g => c3%inn0 )
      type is ( child )
         if ( ( g%i /= 200 ) .or. ( g%j /= 300 ) ) error stop 6_4
      class default
         error stop 7_4
   end select

   c3 = container(gen3(400,500,600))

   c1 = c3
   c2 = c3
   c3 = c3

   select type ( g => c1%inn0 )
      type is ( gen3 )
         if ( ( g%i /= 400 ) .or. ( g%j /= 500 ) .or. ( g%k /= 600 ) ) error stop 8_4
      class default
         error stop 9_4
   end select

   select type ( g => c2%inn0 )
      type is ( gen3 )
         if ( ( g%i /= 400 ) .or. ( g%j /= 500 ) .or. ( g%k /= 600 ) ) error stop 10_4
      class default
         error stop 11_4
   end select

   select type ( g => c3%inn0 )
      type is ( gen3 )
         if ( ( g%i /= 400 ) .or. ( g%j /= 500 ) .or. ( g%k /= 600 ) ) error stop 12_4
      class default
         error stop 13_4
   end select

end program
