! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueScalarAllocatableComponent009.f
! opt variations: -ql -qdefaultpv -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: multiple levels of derived type and polymorphic allocatable components
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

   type level0(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i0
   end type

   type, extends(level0) :: clevel0    ! (4)
      integer(k1), allocatable :: i0_1
   end type

   type level1(k2)    ! (4)
      integer, kind                  :: k2
      class(level0(k2)), allocatable :: l0
      integer(k2), allocatable       :: i1
   end type

   type, extends(level1) :: clevel1    ! (4)
      integer(k2), allocatable :: i1_1
   end type

   type level2(k3)    ! (4)
      integer, kind                  :: k3
      class(level1(k3)), allocatable :: l1
      integer(k3), allocatable       :: i2
   end type

   type, extends(level2) :: clevel2    ! (4)
      integer(k3), allocatable :: i2_1
   end type

   type level3(k4)    ! (4)
      integer, kind                  :: k4
      class(level2(k4)), allocatable :: l2
      integer(k4), allocatable       :: i3
   end type

   type, extends(level3) :: clevel3    ! (4)
      integer(k4), allocatable :: i3_1
   end type

   type level4(k5)    ! (4)
      integer, kind                  :: k5
      class(level3(k5)), allocatable :: l3
      integer(k5), allocatable       :: i4
   end type

   type(level4(4)), pointer :: ll4_1

   contains

      subroutine foo( dtv )
         type(level4(4)), value :: dtv

         print *, dtv%i4
         dtv%i4 = -999
         print *, dtv%i4

         select type ( g => dtv%l3 )
            type is ( level3(4) )
               print *, g%i3
               g%i3 = -999
               print *, g%i3
            type is ( clevel3(4) )
               print *, g%i3, g%i3_1
               g%i3   = -999
               g%i3_1 = -999
               print *, g%i3, g%i3_1
         end select

         select type ( g => dtv%l3%l2 )
            type is ( level2(4) )
               print *, g%i2
               g%i2 = -999
               print *, g%i2
            type is ( clevel2(4) )
               print *, g%i2, g%i2_1
               g%i2   = -999
               g%i2_1 = -999
               print *, g%i2, g%i2_1
         end select

         select type ( g => dtv%l3%l2%l1 )
            type is ( level1(4) )
               print *, g%i1
               g%i1 = -999
               print *, g%i1
            type is ( clevel1(4) )
               print *, g%i1, g%i1_1
               g%i1   = -999
               g%i1_1 = -999
               print *, g%i1, g%i1_1
         end select

         select type ( g => dtv%l3%l2%l1%l0 )
            type is ( level0(4) )
               print *, g%i0
               g%i0 = -999
               print *, g%i0
            type is ( clevel0(4) )
               print *, g%i0, g%i0_1
               g%i0   = -999
               g%i0_1 = -999
               print *, g%i0, g%i0_1
         end select

      end subroutine

end module

program valueScalarAllocatableComponent009
   use m

   type(level4(4)) :: ll4

   ll4 = level4(4)( clevel3(4)( level2(4)( clevel1(4)( clevel0(4)( 1, 2 ) , 3, 4), 5 ), 6, 7), 8 )
   call foo ( ll4 )

   print *, ll4%i4

   select type ( g => ll4%l3 )
      type is ( level3(4) )
         print *, g%i3
      type is ( clevel3(4) )
         print *, g%i3, g%i3_1
   end select

   select type ( g => ll4%l3%l2 )
      type is ( level2(4) )
         print *, g%i2
      type is ( clevel2(4) )
         print *, g%i2, g%i2_1
   end select

   select type ( g => ll4%l3%l2%l1 )
      type is ( level1(4) )
         print *, g%i1
      type is ( clevel1(4) )
         print *, g%i1, g%i1_1
   end select

   select type ( g => ll4%l3%l2%l1%l0 )
      type is ( level0(4) )
         print *, g%i0
      type is ( clevel0(4) )
         print *, g%i0, g%i0_1
   end select

   allocate ( ll4_1, source = level4(4)( clevel3(4)( level2(4)( level1(4)( level0(4)( 11 ) , 12 ), 13 ), 14 , 15 ), 16 ) )

   call foo ( ll4_1 )

   print *, ll4_1%i4

   select type ( g => ll4_1%l3 )
      type is ( level3(4) )
         print *, g%i3
      type is ( clevel3(4) )
         print *, g%i3, g%i3_1
   end select

   select type ( g => ll4_1%l3%l2 )
      type is ( level2(4) )
         print *, g%i2
      type is ( clevel2(4) )
         print *, g%i2, g%i2_1
   end select

   select type ( g => ll4_1%l3%l2%l1 )
      type is ( level1(4) )
         print *, g%i1
      type is ( clevel1(4) )
         print *, g%i1, g%i1_1
   end select

   select type ( g => ll4_1%l3%l2%l1%l0 )
      type is ( level0(4) )
         print *, g%i0
      type is ( clevel0(4) )
         print *, g%i0, g%i0_1
   end select

end program
