! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/valueAttrwAllocCompnt/valueArrayAllocatableComponent010.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

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
!*  DESCRIPTION                : value attribute with derived type containing array allocatable components
!*                                 - type: multiple levels of derived type and unlimited-polymorphic array allocatable components
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

   type level1(n1,k1)    ! (20,4)
      integer, kind            :: k1
      integer, len             :: n1
      integer(k1), allocatable :: i1(:)
   end type

   type, extends(level1) :: clevel1(n2,k2)    ! (20,4,20,4)
      integer, kind            :: k2
      integer, len             :: n2
      integer(k2), allocatable :: i1_1(:)
   end type

   type level2(n3,k3)    ! (20,4)
      integer, kind            :: k3
      integer, len             :: n3
      class(*), allocatable :: l1
      integer(k3), allocatable :: i2(:)
   end type

   type, extends(level2) :: clevel2(n4,k4)    ! (20,4,20,4)
      integer, kind            :: k4
      integer, len             :: n4
      integer(k4), allocatable :: i2_1(:)
   end type

   type level3(n5,k5)    ! (20,4)
      integer, kind            :: k5
      integer, len             :: n5
      class(*), allocatable :: l2
      integer(k5), allocatable :: i3(:)
   end type

   type, extends(level3) :: clevel3(n6,k6)    ! (20,4,20,4)
      integer, kind            :: k6
      integer, len             :: n6
      integer(k6), allocatable :: i3_1(:)
   end type

   type level4(n7,k7)    ! (20,4)
      integer, kind            :: k7
      integer, len             :: n7
      class(*), allocatable :: l3
      integer(k7), allocatable :: i4(:)
   end type

   class(level4(:,4)), pointer :: ll4_1

   contains

      subroutine foo( dtv )
         type(level4(20,4)), value :: dtv

         print *, dtv%i4
         dtv%i4 = -999
         print *, dtv%i4

         select type ( g => dtv%l3 )
            type is ( level3(*,4) )
               print *, g%i3
               g%i3 = -999
               print *, g%i3
               select type ( h => g%l2 )
                  type is ( level2(*,4) )
                     print *, h%i2
                     h%i2 = -999
                     print *, h%i2
                     select type ( j => h%l1 )
                        type is ( level1(*,4) )
                           print *, j%i1
                           j%i1 = -999
                           print *, j%i1
                        type is ( clevel1(*,4,*,4) )
                           print *, j%i1, j%i1_1
                           j%i1   = -999
                           j%i1_1 = -999
                           print *, j%i1, j%i1_1
                     end select
                  type is ( clevel2(*,4,*,4) )
                     print *, h%i2, h%i2_1
                     h%i2   = -999
                     h%i2_1 = -999
                     print *, h%i2, h%i2_1
                     select type ( j => h%l1 )
                        type is ( level1(*,4) )
                           print *, j%i1
                           j%i1 = -999
                           print *, j%i1
                        type is ( clevel1(*,4,*,4) )
                           print *, j%i1, j%i1_1
                           j%i1   = -999
                           j%i1_1 = -999
                           print *, j%i1, j%i1_1
                     end select
               end select
            type is ( clevel3(*,4,*,4) )
               print *, g%i3, g%i3_1
               g%i3 = -999
               g%i3_1 = -999
               print *, g%i3, g%i3_1
               select type ( h => g%l2 )
                  type is ( level2(*,4) )
                     print *, h%i2
                     h%i2 = -999
                     print *, h%i2
                     select type ( j => h%l1 )
                        type is ( level1(*,4) )
                           print *, j%i1
                           j%i1 = -999
                           print *, j%i1
                        type is ( clevel1(*,4,*,4) )
                           print *, j%i1, j%i1_1
                           j%i1   = -999
                           j%i1_1 = -999
                           print *, j%i1, j%i1_1
                     end select
                  type is ( clevel2(*,4,*,4) )
                     print *, h%i2, h%i2_1
                     h%i2   = -999
                     h%i2_1 = -999
                     print *, h%i2, h%i2_1
                     select type ( j => h%l1 )
                        type is ( level1(*,4) )
                           print *, j%i1
                           j%i1 = -999
                           print *, j%i1
                        type is ( clevel1(*,4,*,4) )
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

   type(level4(20,4)) :: ll4

   ll4 = level4(20,4)( clevel3(20,4,20,4)( level2(20,4)( clevel1(20,4,20,4)( (/ 1 /) , (/ 2, 3 /)), (/ 4,5,6 /) ), (/ 7,8,9,10 /), (/ 11,12,13,14,15 /) ), (/ 16,17,18,19,20,21 /) )
   call foo ( ll4 )

   print *, ll4%i4

   select type ( g => ll4%l3 )
      type is ( level3(*,4) )
         print *, g%i3
         select type ( h => g%l2 )
            type is ( level2(*,4) )
               print *, h%i2
               select type ( j => h%l1 )
                  type is ( level1(*,4) )
                     print *, j%i1
                  type is ( clevel1(*,4,*,4) )
                     print *, j%i1, j%i1_1
               end select
            type is ( clevel2(*,4,*,4) )
               print *, h%i2, h%i2_1
               select type ( j => h%l1 )
                  type is ( level1(*,4) )
                     print *, j%i1
                  type is ( clevel1(*,4,*,4) )
                     print *, j%i1, j%i1_1
               end select
         end select
      type is ( clevel3(*,4,*,4) )
         print *, g%i3, g%i3_1
         select type ( h => g%l2 )
            type is ( level2(*,4) )
               print *, h%i2
               select type ( j => h%l1 )
                  type is ( level1(*,4) )
                     print *, j%i1
                  type is ( clevel1(*,4,*,4) )
                     print *, j%i1, j%i1_1
               end select
            type is ( clevel2(*,4,*,4) )
               print *, h%i2, h%i2_1
               select type ( j => h%l1 )
                  type is ( level1(*,4) )
                     print *, j%i1
                  type is ( clevel1(*,4,*,4) )
                     print *, j%i1, j%i1_1
               end select
         end select
   end select

   allocate ( ll4_1, source = level4(20,4)( clevel3(20,4,20,4)( level2(20,4)( clevel1(20,4,20,4)( (/ 11 /) , (/ 12, 13 /)), (/ 14,15,16 /) ), (/ 17,18,19,20 /), (/ 21,22,23,24,25 /)), (/ 26,27,28,29,30,31 /) ) )
   call foo ( ll4_1 )

   print *, ll4_1%i4

   select type ( g => ll4_1%l3 )
      type is ( level3(*,4) )
         print *, g%i3
         select type ( h => g%l2 )
            type is ( level2(*,4) )
               print *, h%i2
               select type ( j => h%l1 )
                  type is ( level1(*,4) )
                     print *, j%i1
                  type is ( clevel1(*,4,*,4) )
                     print *, j%i1, j%i1_1
               end select
            type is ( clevel2(*,4,*,4) )
               print *, h%i2, h%i2_1
               select type ( j => h%l1 )
                  type is ( level1(*,4) )
                     print *, j%i1
                  type is ( clevel1(*,4,*,4) )
                     print *, j%i1, j%i1_1
               end select
         end select
      type is ( clevel3(*,4,*,4) )
         print *, g%i3, g%i3_1
         select type ( h => g%l2 )
            type is ( level2(*,4) )
               print *, h%i2
               select type ( j => h%l1 )
                  type is ( level1(*,4) )
                     print *, j%i1
                  type is ( clevel1(*,4,*,4) )
                     print *, j%i1, j%i1_1
               end select
            type is ( clevel2(*,4,*,4) )
               print *, h%i2, h%i2_1
               select type ( j => h%l1 )
                  type is ( level1(*,4) )
                     print *, j%i1
                  type is ( clevel1(*,4,*,4) )
                     print *, j%i1, j%i1_1
               end select
         end select
   end select

end program
