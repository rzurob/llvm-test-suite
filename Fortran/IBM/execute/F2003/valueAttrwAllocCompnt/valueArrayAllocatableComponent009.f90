!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: multiple levels of derived type and polymorphic array allocatable components
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
      integer, allocatable :: i0(:)
   end type

   type, extends(level0) :: clevel0
      integer, allocatable :: i0_1(:)
   end type

   type level1
      class(level0), allocatable :: l0
      integer, allocatable :: i1(:)
   end type

   type, extends(level1) :: clevel1
      integer, allocatable :: i1_1(:)
   end type

   type level2
      class(level1), allocatable :: l1
      integer, allocatable :: i2(:)
   end type

   type, extends(level2) :: clevel2
      integer, allocatable :: i2_1(:)
   end type

   type level3
      class(level2), allocatable :: l2
      integer, allocatable :: i3(:)
   end type

   type, extends(level3) :: clevel3
      integer, allocatable :: i3_1(:)
   end type

   type level4
      class(level3), allocatable :: l3
      integer, allocatable :: i4(:)
   end type

   type(level4), pointer :: ll4_1

   contains

      subroutine foo( dtv )
         type(level4), value :: dtv

         print *, dtv%i4
         dtv%i4 = -999
         print *, dtv%i4

         select type ( g => dtv%l3 )
            type is ( level3 )
               print *, g%i3
               g%i3 = -999
               print *, g%i3
            type is ( clevel3 )
               print *, g%i3, g%i3_1
               g%i3   = -999
               g%i3_1 = -999
               print *, g%i3, g%i3_1
         end select

         select type ( g => dtv%l3%l2 )
            type is ( level2 )
               print *, g%i2
               g%i2 = -999
               print *, g%i2
            type is ( clevel2 )
               print *, g%i2, g%i2_1
               g%i2   = -999
               g%i2_1 = -999
               print *, g%i2, g%i2_1
         end select

         select type ( g => dtv%l3%l2%l1 )
            type is ( level1 )
               print *, g%i1
               g%i1 = -999
               print *, g%i1
            type is ( clevel1 )
               print *, g%i1, g%i1_1
               g%i1   = -999
               g%i1_1 = -999
               print *, g%i1, g%i1_1
         end select

         select type ( g => dtv%l3%l2%l1%l0 )
            type is ( level0 )
               print *, g%i0
               g%i0 = -999
               print *, g%i0
            type is ( clevel0 )
               print *, g%i0, g%i0_1
               g%i0   = -999
               g%i0_1 = -999
               print *, g%i0, g%i0_1
         end select

      end subroutine

end module

program valueArrayAllocatableComponent009
   use m

   type(level4) :: ll4

   ll4 = level4( clevel3( level2( clevel1( clevel0( (/ 1,2,3 /), (/ 4,5,6 /) ) , (/ 7,8,9 /), (/ 10,11,12 /) ), (/ 13,14,15 /) ), (/ 16,17,18 /), (/ 19,20,21 /)), (/22,23,24/) )
   call foo ( ll4 )

   print *, ll4%i4

   select type ( g => ll4%l3 )
      type is ( level3 )
         print *, g%i3
      type is ( clevel3 )
         print *, g%i3, g%i3_1
   end select

   select type ( g => ll4%l3%l2 )
      type is ( level2 )
         print *, g%i2
      type is ( clevel2 )
         print *, g%i2, g%i2_1
   end select

   select type ( g => ll4%l3%l2%l1 )
      type is ( level1 )
         print *, g%i1
      type is ( clevel1 )
         print *, g%i1, g%i1_1
   end select

   select type ( g => ll4%l3%l2%l1%l0 )
      type is ( level0 )
         print *, g%i0
      type is ( clevel0 )
         print *, g%i0, g%i0_1
   end select

   allocate ( ll4_1, source = level4( clevel3( level2( clevel1( clevel0( (/ 11,12,13 /), (/ 14,15,16 /) ) , (/ 17,18,19 /), (/ 20,21,22 /) ), (/ 23,24,25 /) ), (/ 26,27,28 /), (/ 29,30,31 /)), (/32,33,34/) ) )

   call foo ( ll4_1 )

   print *, ll4_1%i4

   select type ( g => ll4_1%l3 )
      type is ( level3 )
         print *, g%i3
      type is ( clevel3 )
         print *, g%i3, g%i3_1
   end select

   select type ( g => ll4_1%l3%l2 )
      type is ( level2 )
         print *, g%i2
      type is ( clevel2 )
         print *, g%i2, g%i2_1
   end select

   select type ( g => ll4_1%l3%l2%l1 )
      type is ( level1 )
         print *, g%i1
      type is ( clevel1 )
         print *, g%i1, g%i1_1
   end select

   select type ( g => ll4_1%l3%l2%l1%l0 )
      type is ( level0 )
         print *, g%i0
      type is ( clevel0 )
         print *, g%i0, g%i0_1
   end select

end program
