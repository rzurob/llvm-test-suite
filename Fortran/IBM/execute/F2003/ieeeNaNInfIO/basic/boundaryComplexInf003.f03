!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 14, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 311684
!*
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
!*  negative) for complex(4)/(8)/(16) with the D edit descriptor.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(16)     :: rli, rlr
      complex(4)  :: cx1(8)
      complex(8)  :: cx2(8), rliequiv, rlrequiv
      complex(16) :: cx3
      integer     :: i = 0

      integer, parameter :: unit = 11

      character(72), parameter :: myfmt =                             &
     & '(2D1.1,/,2D2.1,/,2D3.2,/,2D4.2,/,2D5.2' //                    &
     & ',/,2D6.2,/,2D7.2,/,2D15.2)'

      equivalence(rli, rliequiv)
      equivalence(rlr, rlrequiv)

      open(unit, file='boundaryComplexInf003.out', action='write')

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
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3

      rliequiv = z'FFF0000000000000' ! negative Inf
      rlrequiv = z'FFF0000000000000' ! negative Inf
      cx3 = (rlr, rli)
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3

      rliequiv = z'7FF0000000000000' ! positive Inf
      rlrequiv = z'FFF0000000000000' ! negative Inf
      cx3 = (rlr, rli)
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3

      close(unit)

      end