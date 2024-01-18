! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn040.f
! opt variations: -ql -qdefaultpv -qreuse=self

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
!*                                 - Deeper levels of allocatable components have generic assignment defined
!*
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

   type level4(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         generic :: assignment(=) => l4assgn
         procedure :: l4assgn => assgn
   end type

   type level3(k2)    ! (4)
      integer, kind                 :: k2
      integer(k2)                   :: i
      type(level4(k2)), allocatable :: l4
   end type

   type level2(k3)    ! (4)
      integer, kind    :: k3
      integer(k3)      :: i
      type(level3(k3)) :: l3
   end type

   type level1(k4)    ! (4)
      integer, kind                 :: k4
      integer(k4)                   :: i
      type(level2(k4)), allocatable :: l2
   end type

   type level0(k5)    ! (4)
      integer, kind                 :: k5
      integer(k5)                   :: i
      type(level1(k5)), allocatable :: l1
   end type

   contains

      subroutine assgn ( a, b )
         class(level4(4)), intent(out) :: a
         class(level4(4)), intent(in)  :: b

         a%i = b%i
         print *, 'assgn'
      end subroutine

end module

program genericAssignmentDtIntrinAssgn040
   use m

   type(level0(4)) :: l01, l02, l03

   type(level1(4)) :: l10
   type(level2(4)) :: l20
   type(level3(4)) :: l30

   allocatable :: l02
   pointer :: l03

   allocate ( l02, l03 )

   l01 = level0(4)( 1, level1(4)(2, level2(4)(3, level3(4)( 4, level4(4) ( 5 ) ) ) ) )
   print *, l01%i, l01%l1%i, l01%l1%l2%i, l01%l1%l2%l3%i, l01%l1%l2%l3%l4%i

   l02 = l01
   print *, l02%i, l02%l1%i, l02%l1%l2%i, l02%l1%l2%l3%i, l02%l1%l2%l3%l4%i

   l03 = l02
   print *, l03%i, l03%l1%i, l03%l1%l2%i, l03%l1%l2%l3%i, l03%l1%l2%l3%l4%i

   l10 = l01%l1
   print *, l10%i, l10%l2%i, l10%l2%l3%i, l10%l2%l3%l4%i

   l20 = l01%l1%l2
   print *, l20%i, l20%l3%i, l20%l3%l4%i

   l20 = l10%l2
   print *, l20%i, l20%l3%i, l20%l3%l4%i

   l30 = l01%l1%l2%l3
   print *, l30%i, l30%l4%i

end program
