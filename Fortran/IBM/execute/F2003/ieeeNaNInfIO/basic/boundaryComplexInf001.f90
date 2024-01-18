!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : boundaryComplexInf001.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 14, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Boundary testing for different widths of the output field.
!*  This testcase covers output of IEEE Infinity (positive vs.
!*  negative) for complex(4)/(8)/(16) with the F edit descriptor.
!*
!*  ADDITIONAL NOTES:
!*  -----------------
!*  UNTIL DEFECT 323799 IS FIXED, THIS TESTCASE WILL CONTINUE TO FAIL AT
!*  THE VERIFICATION STEP WITH A SINGLE SPACE DIFFERENCE ON LINES 19 AND 46.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic 
      implicit none

      real(16)     :: rli, rlr
      complex(4)  :: cx1(9)
      complex(8)  :: cx2(9), rliequiv, rlrequiv
      complex(16) :: cx3
      integer     :: i = 0

      integer, parameter :: unit = 11

      character(72), parameter :: myfmt =                             & 
     & '(2F0.0,/,2F1.1,/,2F2.1,/,2F3.2,/,2F4.2,/,2F5.2' //            &
     & ',/,2F6.2,/,2F7.2,/,2F15.2)'

      equivalence(rli, rliequiv)
      equivalence(rlr, rlrequiv)

      open(unit, file='boundaryComplexInf001.out', action='write')
      
      ! Write out complex(4) Infinity ( positive and negative )

      cx1 = (z'7F800000',z'7F800000') ! positive Inf
      write(unit, fmt=myfmt) cx1

      cx1 = (z'FF800000',z'FF800000') ! negative Inf
      write(unit, fmt=myfmt) cx1

      cx1 = (z'FF800000',z'7F800000') ! negative/positive Inf
      write(unit, fmt=myfmt) cx1

      ! Write out complex(8) Infinity ( positive and negative )

      cx2 = (z'7FF0000000000000', z'7FF0000000000000') ! positive Inf
      write(unit, fmt=myfmt) cx2

      cx2 = (z'FFF0000000000000', z'FFF0000000000000') ! negative Inf
      write(unit, fmt=myfmt) cx2

      cx2 = (z'FFF0000000000000', z'7FF0000000000000') ! negative/positive Inf
      write(unit, fmt=myfmt) cx2

      ! Write out complex(16) Infinity ( positive and negative )

      rli = z'7FF0000000000000' ! positive Inf
      rlr = z'7FF0000000000000' ! positive Inf
      cx3 = (rlr, rli)
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3

      rliequiv = z'FFF0000000000000' ! negative Inf
      rlrequiv = z'FFF0000000000000' ! negative Inf
      cx3 = (rlr, rli)
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3
 
      rliequiv = z'7FF0000000000000' ! positive Inf
      rlrequiv = z'FFF0000000000000' ! negative Inf
      cx3 = (rlr, rli)
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3

      close(unit)

      end
