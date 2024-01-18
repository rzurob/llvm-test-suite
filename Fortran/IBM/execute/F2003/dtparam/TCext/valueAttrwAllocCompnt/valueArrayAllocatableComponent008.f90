! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueArrayAllocatableComponent008.f
! opt variations: -ql -qdefaultpv -qreuse=none

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
!*                                 - type: multiple levels of derived type and array allocatable components
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
      integer(k1), allocatable :: i0(:)
   end type

   type level1(k2)    ! (4)
      integer, kind                 :: k2
      type(level0(k2)), allocatable :: l0
      integer(k2), allocatable      :: i1(:)
   end type

   type level2(k3)    ! (4)
      integer, kind                 :: k3
      type(level1(k3)), allocatable :: l1
      integer(k3), allocatable      :: i2(:)
   end type

   type level3(k4)    ! (4)
      integer, kind                 :: k4
      type(level2(k4)), allocatable :: l2
      integer(k4), allocatable      :: i3(:)
   end type

   type level4(k5)    ! (4)
      integer, kind                 :: k5
      type(level3(k5)), allocatable :: l3
      integer(k5), allocatable      :: i4(:)
   end type

   type(level4(4)), pointer :: ll4_1

   contains

      subroutine foo( dtv )
         type(level4(4)), value :: dtv

         print *, dtv%i4, dtv%l3%i3, dtv%l3%l2%i2, dtv%l3%l2%l1%i1, dtv%l3%l2%l1%l0%i0

         dtv%i4 = -999
         dtv%l3%i3 = -999
         dtv%l3%l2%i2 = -999
         dtv%l3%l2%l1%i1 = -999
         dtv%l3%l2%l1%l0%i0 = -999

         print *, dtv%i4, dtv%l3%i3, dtv%l3%l2%i2, dtv%l3%l2%l1%i1, dtv%l3%l2%l1%l0%i0

      end subroutine

end module

program valueArrayAllocatableComponent008
   use m

   type(level4(4)) :: ll4

   ll4 = level4(4)( level3(4)( level2(4)( level1(4)( level0(4)( (/ 1, 2, 3 /) ) , (/4,5,6/) ), (/7,8,9/) ), (/10,11,12/) ), (/13,14/) )
   call foo ( ll4 )

   print *, ll4%i4, ll4%l3%i3, ll4%l3%l2%i2, ll4%l3%l2%l1%i1, ll4%l3%l2%l1%l0%i0

   allocate ( ll4_1, source = level4(4)( level3(4)( level2(4)( level1(4)( level0(4)( (/ 11, 12, 13 /) ) , (/14,15,16/) ), (/17,18,19/) ), (/20,21,22/) ), (/23,24,25/) ) )

   call foo ( ll4_1 )

   print *, ll4_1%i4, ll4_1%l3%i3, ll4_1%l3%l2%i2, ll4_1%l3%l2%l1%i1, ll4_1%l3%l2%l1%l0%i0

end program
