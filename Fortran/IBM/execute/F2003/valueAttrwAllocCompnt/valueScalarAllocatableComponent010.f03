!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: multiple levels of derived type and unlimited-polymorphic allocatable components
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

   type level1
      integer, allocatable :: i1
   end type

   type, extends(level1) :: clevel1
      integer, allocatable :: i1_1
   end type

   type level2
      class(*), allocatable :: l1
      integer, allocatable :: i2
   end type

   type, extends(level2) :: clevel2
      integer, allocatable :: i2_1
   end type

   type level3
      class(*), allocatable :: l2
      integer, allocatable :: i3
   end type

   type, extends(level3) :: clevel3
      integer, allocatable :: i3_1
   end type

   type level4
      class(*), allocatable :: l3
      integer, allocatable :: i4
   end type

   class(level4), pointer :: ll4_1

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
               select type ( h => g%l2 )
                  type is ( level2 )
                     print *, h%i2
                     h%i2 = -999
                     print *, h%i2
                     select type ( j => h%l1 )
                        type is ( level1 )
                           print *, j%i1
                           j%i1 = -999
                           print *, j%i1
                        type is ( clevel1 )
                           print *, j%i1, j%i1_1
                           j%i1   = -999
                           j%i1_1 = -999
                           print *, j%i1, j%i1_1
                     end select
                  type is ( clevel2 )
                     print *, h%i2, h%i2_1
                     h%i2   = -999
                     h%i2_1 = -999
                     print *, h%i2, h%i2_1
                     select type ( j => h%l1 )
                        type is ( level1 )
                           print *, j%i1
                           j%i1 = -999
                           print *, j%i1
                        type is ( clevel1 )
                           print *, j%i1, j%i1_1
                           j%i1   = -999
                           j%i1_1 = -999
                           print *, j%i1, j%i1_1
                     end select
               end select
            type is ( clevel3 )
               print *, g%i3, g%i3_1
               g%i3 = -999
               g%i3_1 = -999
               print *, g%i3, g%i3_1
               select type ( h => g%l2 )
                  type is ( level2 )
                     print *, h%i2
                     h%i2 = -999
                     print *, h%i2
                     select type ( j => h%l1 )
                        type is ( level1 )
                           print *, j%i1
                           j%i1 = -999
                           print *, j%i1
                        type is ( clevel1 )
                           print *, j%i1, j%i1_1
                           j%i1   = -999
                           j%i1_1 = -999
                           print *, j%i1, j%i1_1
                     end select
                  type is ( clevel2 )
                     print *, h%i2, h%i2_1
                     h%i2   = -999
                     h%i2_1 = -999
                     print *, h%i2, h%i2_1
                     select type ( j => h%l1 )
                        type is ( level1 )
                           print *, j%i1
                           j%i1 = -999
                           print *, j%i1
                        type is ( clevel1 )
                           print *, j%i1, j%i1_1
                           j%i1   = -999
                           j%i1_1 = -999
                           print *, j%i1, j%i1_1
                     end select
               end select
         end select

      end subroutine

end module

program valueScalarAllocatableComponent010
   use m

   type(level4) :: ll4

   ll4 = level4( clevel3( level2( clevel1( 1, 2), 3 ), 4, 5), 6 )
   call foo ( ll4 )

   print *, ll4%i4

   select type ( g => ll4%l3 )
      type is ( level3 )
         print *, g%i3
         select type ( h => g%l2 )
            type is ( level2 )
               print *, h%i2
               select type ( j => h%l1 )
                  type is ( level1 )
                     print *, j%i1
                  type is ( clevel1 )
                     print *, j%i1, j%i1_1
               end select
            type is ( clevel2 )
               print *, h%i2, h%i2_1
               select type ( j => h%l1 )
                  type is ( level1 )
                     print *, j%i1
                  type is ( clevel1 )
                     print *, j%i1, j%i1_1
               end select
         end select
      type is ( clevel3 )
         print *, g%i3, g%i3_1
         select type ( h => g%l2 )
            type is ( level2 )
               print *, h%i2
               select type ( j => h%l1 )
                  type is ( level1 )
                     print *, j%i1
                  type is ( clevel1 )
                     print *, j%i1, j%i1_1
               end select
            type is ( clevel2 )
               print *, h%i2, h%i2_1
               select type ( j => h%l1 )
                  type is ( level1 )
                     print *, j%i1
                  type is ( clevel1 )
                     print *, j%i1, j%i1_1
               end select
         end select
   end select

   allocate ( ll4_1, source = level4( clevel3( level2( level1( 11 ), 12 ), 13 , 14 ), 15 ) )
   call foo ( ll4_1 )

   print *, ll4_1%i4

   select type ( g => ll4_1%l3 )
      type is ( level3 )
         print *, g%i3
         select type ( h => g%l2 )
            type is ( level2 )
               print *, h%i2
               select type ( j => h%l1 )
                  type is ( level1 )
                     print *, j%i1
                  type is ( clevel1 )
                     print *, j%i1, j%i1_1
               end select
            type is ( clevel2 )
               print *, h%i2, h%i2_1
               select type ( j => h%l1 )
                  type is ( level1 )
                     print *, j%i1
                  type is ( clevel1 )
                     print *, j%i1, j%i1_1
               end select
         end select
      type is ( clevel3 )
         print *, g%i3, g%i3_1
         select type ( h => g%l2 )
            type is ( level2 )
               print *, h%i2
               select type ( j => h%l1 )
                  type is ( level1 )
                     print *, j%i1
                  type is ( clevel1 )
                     print *, j%i1, j%i1_1
               end select
            type is ( clevel2 )
               print *, h%i2, h%i2_1
               select type ( j => h%l1 )
                  type is ( level1 )
                     print *, j%i1
                  type is ( clevel1 )
                     print *, j%i1, j%i1_1
               end select
         end select
   end select

end program