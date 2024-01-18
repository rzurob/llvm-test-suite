! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/bindc1/functional/bindc016.f
! opt variations: -qnok -ql

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
!*                                        Derived type containing BIND(C) procedure pointer component with private attribute
!*                                        and use procedure pointer as dummy argument
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
      subroutine setandprint1(r1, r2) bind(c)
         import C_FLOAT
         real(C_FLOAT), intent(inout) :: r1
         real(C_FLOAT), intent(in) :: r2
      end subroutine
   end interface

   interface
      subroutine setandprint2(r1, r2) bind(c)
         import C_FLOAT
         real(C_FLOAT), intent(inout) :: r1
         real(C_FLOAT), intent(in) :: r2
      end subroutine
   end interface

   type base(k1)    ! (4)
      integer, kind :: k1
      real(C_FLOAT) :: r1 = 0.0
      real(C_FLOAT) :: r2 = 0.0
      procedure(setandprint1), pointer, private, nopass :: pp =>null()
   end type

   contains

      subroutine setbasepp ( dtv, pp )
         type(base(4)), intent(inout) :: dtv
         procedure(setandprint2), pointer :: pp

         dtv%pp => pp

      end subroutine

      subroutine callbasepp ( dtv, r1, r2 )
         type(base(4)), intent(inout) :: dtv
         real(C_FLOAT), intent(in) :: r1, r2

         call dtv%pp( dtv%r1, r1 )
         call dtv%pp( dtv%r2, r2 )

      end subroutine

end module

use m

   type(base(4)) :: b1
   type(base(4)), allocatable, target :: b2
   type(base(4)), pointer :: b3

   procedure(setandprint1), pointer :: mypp
   logical :: precision_r4

   allocate ( b2 )
   b3 => b2

   mypp => setandprint1

   call setbasepp(b1, mypp)
   call callbasepp(b1, 101.0_C_FLOAT, 102.0_C_FLOAT)

   if ( ( .not. precision_r4(b1%r1,101.0_C_FLOAT) ) .or. ( .not. precision_r4(b1%r2,102.0_C_FLOAT) ) )   error stop 1_4

   mypp => setandprint2

   call setbasepp( b2, mypp )
   call callbasepp(b2, 201.0_C_FLOAT, 202.0_C_FLOAT)

   if ( ( .not. precision_r4(b2%r1,201.0_C_FLOAT) ) .or. ( .not. precision_r4(b2%r2,202.0_C_FLOAT) ) )   error stop 2_4

   b3 => b2

   call callbasepp(b3, 301.0_C_FLOAT, 302.0_C_FLOAT)

   if ( ( .not. precision_r4(b2%r1,301.0_C_FLOAT) ) .or. ( .not. precision_r4(b2%r2,302.0_C_FLOAT) ) .or. &
        ( .not. precision_r4(b3%r1,301.0_C_FLOAT) ) .or. ( .not. precision_r4(b3%r2,302.0_C_FLOAT) ) )   error stop 3_4

   mypp => setandprint1
   call setbasepp( b3, mypp )

   call callbasepp(b3, 401.0_C_FLOAT, 402.0_C_FLOAT)

   if ( ( .not. precision_r4(b2%r1,401.0_C_FLOAT) ) .or. ( .not. precision_r4(b2%r2,402.0_C_FLOAT) ) .or. &
        ( .not. precision_r4(b3%r1,401.0_C_FLOAT) ) .or. ( .not. precision_r4(b3%r2,402.0_C_FLOAT) ) )   error stop 4_4

end
