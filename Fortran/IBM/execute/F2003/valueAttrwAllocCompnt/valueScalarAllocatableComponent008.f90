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
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: multiple levels of derived type and allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
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

   type level0
      integer, allocatable :: i0
   end type

   type level1
      type(level0), allocatable :: l0
      integer, allocatable :: i1
   end type

   type level2
      type(level1), allocatable :: l1
      integer, allocatable :: i2
   end type

   type level3
      type(level2), allocatable :: l2
      integer, allocatable :: i3
   end type

   type level4
      type(level3), allocatable :: l3
      integer, allocatable :: i4
   end type

   type(level4), pointer :: ll4_1

   contains

      subroutine foo( dtv )
         type(level4), value :: dtv

         print *, dtv%i4, dtv%l3%i3, dtv%l3%l2%i2, dtv%l3%l2%l1%i1, dtv%l3%l2%l1%l0%i0

         dtv%i4 = -999
         dtv%l3%i3 = -999
         dtv%l3%l2%i2 = -999
         dtv%l3%l2%l1%i1 = -999
         dtv%l3%l2%l1%l0%i0 = -999

         print *, dtv%i4, dtv%l3%i3, dtv%l3%l2%i2, dtv%l3%l2%l1%i1, dtv%l3%l2%l1%l0%i0

      end subroutine

end module

program valueScalarAllocatableComponent008
   use m

   type(level4) :: ll4

   ll4 = level4( level3( level2( level1( level0( 1 ) , 2 ), 3 ), 4 ), 5 )
   call foo ( ll4 )

   print *, ll4%i4, ll4%l3%i3, ll4%l3%l2%i2, ll4%l3%l2%l1%i1, ll4%l3%l2%l1%l0%i0

   allocate ( ll4_1, source = level4( level3( level2( level1( level0( 11 ) , 12 ), 13 ), 14 ), 15 ) )

   call foo ( ll4_1 )

   print *, ll4_1%i4, ll4_1%l3%i3, ll4_1%l3%l2%i2, ll4_1%l3%l2%l1%i1, ll4_1%l3%l2%l1%l0%i0

end program
