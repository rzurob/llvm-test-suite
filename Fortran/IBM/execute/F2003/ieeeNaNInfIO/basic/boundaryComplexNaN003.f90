!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : boundaryComplexNaN003.f
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
!*  This testcase covers output of IEEE NaN (quiet vs. signaling &
!*  positive vs. negative) for complex(4)/(8)/(16) with the D edit descriptor.
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

      open(unit, file='boundaryComplexNaN003.out', action='write')
      
      ! Write out complex(4) quiet NaN ( positive and negative )

      cx1 = (z'7FFFFFFF',z'7FFFFFFF') ! positive NaN(Q)
      write(unit, fmt=myfmt) cx1

      cx1 = (z'FFFFFFFF',z'FFFFFFFF') ! negative NaN(Q)
      write(unit, fmt=myfmt) cx1

      ! Write out complex(8) quiet NaN ( positive and negative )

      cx2 = (z'7FFFFFFFFFFFFFFF', z'7FFFFFFFFFFFFFFF') ! positive NaN(Q)
      write(unit, fmt=myfmt) cx2

      cx2 = (z'FFFFFFFFFFFFFFFF', z'FFFFFFFFFFFFFFFF') ! negative NaN(Q)
      write(unit, fmt=myfmt) cx2

      ! Write out complex(16) quiet NaN ( positive and negative )

      rli = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rlr = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      cx3 = (rlr, rli)
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3

      rliequiv = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rlrequiv = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx3 = (rlr, rli)
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3
 

     !*********************************************************


      ! Write out complex(4) signaling NaN ( positive and negative )

      cx1 = (z'7FBFFFFF', z'7FBFFFFF') ! positive NaN(S)
      write(unit, fmt=myfmt) cx1

      cx1 = (z'FFBFFFFF', z'FFBFFFFF') ! negative NaN(S)
      write(unit, fmt=myfmt) cx1

      ! Write out complex(8) signaling NaN ( positive and negative )

      cx2 = (z'7FF7FFFFFFFFFFFF', z'7FF7FFFFFFFFFFFF') ! positive NaN(S)
      write(unit, fmt=myfmt) cx2

      cx2 = (z'FFF7FFFFFFFFFFFF', z'FFF7FFFFFFFFFFFF') ! negative NaN(Q)
      write(unit, fmt=myfmt) cx2

      ! Write out complex(16) signaling NaN ( positive and negative )

      rli = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rlr = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      cx3 = (rlr, rli)
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3

      rliequiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rlrequiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx3 = (rlr, rli)
      write(unit, fmt=myfmt) cx3, cx3, cx3, cx3, cx3, cx3, cx3, cx3

      close(unit)

      end
