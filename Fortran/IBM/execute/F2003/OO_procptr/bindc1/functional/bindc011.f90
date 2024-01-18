!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 06/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure Pointer pointing at C functions with BIND(C) interface subroutine
!*                                        - dummy argument function return with derived BIND(C) types array explicit shape/assumed size
!*                                          and actual arg being array section with subscript triplet or vector subscript
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

   use ISO_C_BINDING

   type, bind(c) :: base
      integer(C_INT) :: i
      real(C_DOUBLE) :: r
      character(kind=C_CHAR) :: c(3)
   end type

   interface
      subroutine print1(dtv) bind(c)
         import base
         type(base), intent(in) :: dtv(3)
      end subroutine
   end interface

   interface
      type(base) function add ( dtv1, dtv2 ) bind(c)
         import base
         type(base), intent(in) :: dtv1(3), dtv2(3,*)
      end function
   end interface

end module

program bindc011
   use m

   type(base) :: b1(3), b12(3)
   type(base) :: b2(6)
   procedure(print1), pointer, bind(c) :: p1
   procedure(add), pointer, bind(c) :: p2

   logical :: precision_r4


   b1 = (/ base( 101_C_INT, 1001.0_C_DOUBLE, (/ C_CHAR_"a", C_CHAR_"b", C_CHAR_"c" /) ), &
           base( 102_C_INT, 1002.0_C_DOUBLE, (/ C_CHAR_"d", C_CHAR_"e", C_CHAR_"f" /) ), &
           base( 103_C_INT, 1003.0_C_DOUBLE, (/ C_CHAR_"g", C_CHAR_"h", C_CHAR_"i" /) )  &
        /)

   b2 = (/ base( 201_C_INT, 2001.0_C_DOUBLE, (/ C_CHAR_"A", C_CHAR_"B", C_CHAR_"C" /) ), &
           base( 202_C_INT, 2002.0_C_DOUBLE, (/ C_CHAR_"D", C_CHAR_"E", C_CHAR_"F" /) ), &
           base( 203_C_INT, 2003.0_C_DOUBLE, (/ C_CHAR_"G", C_CHAR_"H", C_CHAR_"I" /) ), &
           base( 204_C_INT, 2004.0_C_DOUBLE, (/ C_CHAR_"a", C_CHAR_"b", C_CHAR_"c" /) ), &
           base( 205_C_INT, 2005.0_C_DOUBLE, (/ C_CHAR_"d", C_CHAR_"e", C_CHAR_"f" /) ), &
           base( 206_C_INT, 2006.0_C_DOUBLE, (/ C_CHAR_"g", C_CHAR_"h", C_CHAR_"i" /) )  &
        /)

   p1 => print1

   call p1(b1)
   call p1(b2(1:3))
   call p1(b2(1:6:2))
   call p1(b2((/1,6,1/)))

   p2 => add

   b12 = p2( b1((/1,2,3/)) , b2)   !<- intrinsic assignment for array

   if ( ( b12(1)%i /= 302 ) .or. ( .not. precision_r4(b12(1)%r, 306.0_C_DOUBLE) ) .or. ( b12(1)%c(1) /= 'g' ) .or. &
        ( b12(1)%c(2) /= 'h' ) .or. ( b12(1)%c(3) /= 'i' )  .or.                                                   &
        ( b12(2)%i /= 302 ) .or. ( .not. precision_r4(b12(2)%r, 306.0_C_DOUBLE) ) .or. ( b12(2)%c(1) /= 'g' ) .or. &
        ( b12(2)%c(2) /= 'h' ) .or. ( b12(2)%c(3) /= 'i' ) .or.                                                    &
        ( b12(3)%i /= 302 ) .or. ( .not. precision_r4(b12(3)%r, 306.0_C_DOUBLE) ) .or. ( b12(3)%c(1) /= 'g' ) .or. &
        ( b12(3)%c(2) /= 'h' ) .or. ( b12(3)%c(3) /= 'i' ) ) error stop 1_4

end
