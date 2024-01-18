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
!*                                        Non-bind(c) derived-type contains multiple nopass bind(c) procedure pointer
!*                                        subroutine and function procedures, and within C functions, calling another C function
!*                                        derived type being allocatable (target), pointer, with implicit variable as function result
!*                                        for procedure pointer (function)
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

   interface
      subroutine print (i,j,k) bind(c, name='printthem')
         import C_FLOAT
         real(C_FLOAT), intent(in), value :: i,j,k
      end subroutine
   end interface

   interface
      function sum (i,j,k) bind(c, name='sumthem')
         import C_FLOAT
         real(C_FLOAT) sum
         real(C_FLOAT), intent(in), value :: i,j,k
      end function
   end interface

   interface
      function subtract (i,j,k) bind(c, name='subtractthem')
         import C_FLOAT
         real(C_FLOAT) sum
         real(C_FLOAT), intent(in), value :: i,j,k
      end function
   end interface

   interface
      subroutine inf1 (r1,r2,r3) bind(c)
         import C_FLOAT
         real(C_FLOAT), intent(in), value :: r1,r2,r3
      end subroutine
   end interface

   interface
      real(C_FLOAT) function inf2 (r1,r2,r3) bind(c)
         import C_FLOAT
         real(C_FLOAT), intent(in), value :: r1,r2,r3
      end function
   end interface

   type base
      real(C_FLOAT) :: i,j,k
      procedure(inf1), pointer, nopass :: pp1
      procedure(inf2), pointer, nopass :: pp2
      procedure(inf2), pointer, nopass :: pp3
   end type

end module

   use m
   implicit real(C_FLOAT) (a-b)

   type(base) :: b1
   type(base), allocatable, target :: b2
   type(base), pointer :: b3

   logical :: precision_r4

   b1 = base( 10.0_C_FLOAT, 11.0_C_FLOAT, 12.0_C_FLOAT, null(), null(), null() )
   allocate ( b2 )
   b2 = base( 20.0_C_FLOAT, 21.0_C_FLOAT, 22.0_C_FLOAT, null(), null(), null() )

   b1%pp1 => print
   b1%pp2 => sum
   b1%pp3 => subtract

   call b1%pp1 ( b1%i, b1%j, b1%k )
   a11 = b1%pp2 ( b1%k, b1%j, b1%i )
   a12 = b1%pp3 ( b1%k, b1%j, b1%i )

   if ( ( .not. precision_r4( a11, 33.0 ) ) .or. ( .not. precision_r4( a12, -33.0 ) )  ) error stop 1_4

   b2%pp1 => b1%pp1
   b2%pp2 => b1%pp3
   b2%pp3 => b1%pp2

   call b2%pp1  ( b2%i, b2%j, b2%k )
   a21 = b2%pp2 ( b2%i, b2%j, b2%k )
   a22 = b2%pp3 ( b2%k, b2%j, b2%i )

   if ( ( .not. precision_r4( a21, -63.0 ) ) .or. ( .not. precision_r4( a22, 63.0 ) )  ) error stop 2_4

   b3 => b2

   call b3%pp1  ( b3%i, b3%i, b3%i )
   a31 = b3%pp2 ( b3%j, b3%j, b3%j )
   a32 = b3%pp3 ( b3%k, b3%k, b3%k )

   if ( ( .not. precision_r4( a31, -63.0 ) ) .or. ( .not. precision_r4( a32, 66.0 ) )  ) error stop 3_4

end

