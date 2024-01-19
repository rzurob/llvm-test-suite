!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 16, 2006
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
!*  Test a mixture of input and output values containing IEEE exceptional
!*  specification, with real or complex of kind=8. Try combination of
!*  F,E,D,EN,ES,G, and Q editing in this testcase.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(8)  :: rl1 = 0.0, rl2 = 0.0
      complex(8) :: cx1
      character(10) :: cc1, cc2
      integer :: ii, jj

      integer, parameter :: out = 11, in = 12

      open(in, file='mixNaNInfIO003.dat', action='read')
      open(out, file='mixNaNInfIO003.out', action='write')

      read(in, '(2a3, f3.1, e3.0, i1, g3.1, ES7.7)')                   &
     & cc1, cc2, rl1, rl2, ii, cx1
      write(out, '(2a4, f7.1, e9.0, i1, g7.2, ES10.7)')                &
     & cc1, cc2, rl1, rl2, ii, cx1

      read(in, '(2a3, d3.1, q3.0, i1, es3.1, En7.7)')                  &
     &  cc1, cc2, rl1, rl2, ii, cx1
      write(out, '(2a4, d7.1, q9.0, i1, es8.2, En10.7)')               &
     &  cc1, cc2, rl1, rl2, ii, cx1

      read(in, '(a3, a7, f7.1, e8.5, i1, g3.1, ES6.0)')                &
     & cc1, cc2, rl1, rl2, ii, cx1
      write(out, '(2a7, f7.1, e9.0, i1, g7.2, ES10.7)')                &
     & cc1, cc2, rl1, rl2, ii, cx1

      read(in, '(a3, a7, d7.1, q8.5, i1, es3.1, EN6.0)')               &
     & cc1, cc2, rl1, rl2, ii, cx1
      write(out, '(2a7, d7.1, q9.0, i1, es8.2, EN10.7)')               &
     & cc1, cc2, rl1, rl2, ii, cx1

      read(in, '(a3, a7, f7.1, e9.5, i1, g3.1, ES7.0)')                &
     & cc1, cc2, rl1, rl2, ii, cx1
      write(out, '(2a7, f7.1, e9.0, i1, g7.2, ES10.7)')                &
     & cc1, cc2, rl1, rl2, ii, cx1

      read(in, '(a3, a7, d7.1, q9.5, i1, es3.1, EN7.0)')               &
     & cc1, cc2, rl1, rl2, ii, cx1
      write(out, '(2a7, d7.1, q9.0, i1, es8.2, EN10.7)')               &
     & cc1, cc2, rl1, rl2, ii, cx1


      read(in, '(a4, f8.1, e9.4, i1, g3.1, ES9.0)')                    &
     & cc1, rl1, rl2, ii, cx1
      write(out, '(a4, f8.1, e9.4, i1, g7.2, ES7.0)')                  &
     & cc1, rl1, rl2, ii, cx1

      read(in, '(a4, d8.1, q9.4, i1, es3.1, EN9.0)')                   &
     & cc1, rl1, rl2, ii, cx1
      write(out, '(a4, d8.1, q9.4, i1, es8.2, EN7.0)')                 &
     & cc1, rl1, rl2, ii, cx1


      read(in, '(i2, a3, f9.1, e9.4, i1, g3.1, ES9.0)')                &
     & jj, cc1, rl1, rl2, ii, cx1
      write(out, '(i3, a4, f8.1, e9.4, i1, g7.2, ES7.0)')              &
     & jj, cc1, rl1, rl2, ii, cx1

      read(in, '(i2, a3, d9.1, q9.4, i1, es3.1, en9.0)')               &
     & jj, cc1, rl1, rl2, ii, cx1
      write(out, '(i3, a4, d8.1, q9.4, i1, es8.2, en7.0)')             &
     & jj, cc1, rl1, rl2, ii, cx1


      read(in, '(a4, f3.1, e6.4, g3.1, a1, ES9.0)')                    &
     & cc1, rl1, cx1, cc2, rl2
      write(out, '(a4, f4.1, e6.4, g7.2, a1, a1, ES9.9)')              &
     & cc1, rl1, cx1, ' ', cc2, rl2


      read(in, '(a4, f4.1, e7.4, g4.1, a1, ES9.0)')                    &
     & cc1, rl1, cx1, cc2, rl2
      write(out, '(a4, f5.1, e8.4, g7.2, a1, a1, ES9.9)')              &
     & cc1, rl1, cx1, ' ', cc2, rl2

      close(in)
      close(out)

      end
