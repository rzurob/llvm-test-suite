!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mixNaNInfIO001.f
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
!*  Test a mixture of input and output values containing IEEE exceptional
!*  specification, with objects of type real or complex. Try F editing
!*  in this testcase.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic 
      implicit none
      
      real(4)  :: rl1 = 0.0, rl11 = 0.0, rl111 = 0.0
      real(8)  :: rl2
      real(16) :: rl3

      complex(4) :: cx1
      complex(8) :: cx2
      complex(16) :: cx3

      integer :: ii

      integer, parameter :: out = 11, in = 12
      
      open(in, file='mixNaNInfIO001.dat', action='read')
      open(out, file='mixNaNInfIO001.out', action='write')
      
      read(in,'(2f4.4)') rl1, rl2
      write(out, '(f8.2, f6.3)') rl1, rl2
      
      read(in,'(f3.1, f6.5, f3.3)') rl1, rl2, rl3
      write(out, '(3F6.1)') rl1, rl2, rl3

      read(in,'(2f3.1, f3.1)') rl1, rl2, rl3
      write(out, '(3F6.1)') rl1, rl2, rl3
      
      read(in,'(f3.1, f6.0, f4.2)') rl1, rl2, rl3
      write(out, '(3F6.1, f4.1)') rl1, rl2, rl3, rl2      

      read(in,'(f4.2, f7.0)') rl1, rl2
      write(out, '(2F10.1)') rl1, rl2

      read(in,'(f4.4, i2, f8.0)') rl1, ii, rl2
      write(out, '(F10.1, i5, f8.1)') rl1, ii, rl2

      read(in, '(f13.10, f5.2)') rl1, rl2
      write(out, '(2F10.2)') rl1, rl2

      read(in, '(f4.2, f13.2)') rl3, rl2
      write(out, '(2F10.2)') rl3, rl2

      read(in, '(f15.1, i3)') rl2, ii
      write(out, '(F15.1, i3)') rl2, ii

      read(in, '(i2, f25.1)') ii, rl1
      write(out, '(i3,F15.1)') ii, rl1

      read(in, '(i2, f15.1, f4.4, f3.3)') ii, rl1, rl2, rl3
      write(out, '(i3,3F15.1)') ii, rl1, rl2, rl3
      
      read(in, '(f8.8, 3f3.0, f4.2)') rl1, rl2, rl3, rl11, rl111
      write(out, '(5f9.0)')  rl1, rl2, rl3, rl11, rl111

      read(in, '(f9.8,2f4.2,f4.2)') rl1, rl2, rl3, rl11
      write(out, '(4f10.2)') rl1, rl2, rl3, rl11

      read(in, '(f4.1,f7.2,f4.2, f5.1)') rl1, rl2, rl3, rl11
      write(out, '(4f10.2)') rl1, rl2, rl3, rl11
      

      read(in, '(2f4.2)') cx1
      write(out, '(2f8.3)') cx1

      ! IBM extension allows field width to be terminated by a comma on input.
      read(in, '(f4.1, f7.1, f3.1)') cx2, rl1
      write(out, '(3f8.2)') cx2, rl1

      ! IBM extension allows field width to be terminated by a comma on input.
      read(in, '(f3.1, f6.1, f3.1)') cx2, rl1
      write(out, '(3f8.2)') cx2, rl1


      read(in, '(f3.0, f7.1, f4.1)') cx3, rl2
      write(out, '(3f8.2)') cx3, rl2
      
      read(in, '(f5.4, f7.4)') cx2
      write(out, '(2f10.1)') cx2

      read(in, '(f9.2, i4, f10.0)') rl3, ii, rl2
      write(out, '(f10.2, i2, f10.2)') rl3, ii, rl2

      read(in, '(2f14.2)') cx1
      write(out, '(2f14.2)') cx1

      read(in, '(f5.2, f15.10)') cx2
      write(out, '(2f6.2)') cx2

      read(in, '(f15.2, i4)') rl2, ii
      write(out, '(f6.2, i4)') rl2, ii

      read(in, '(i3,f25.5)') ii, rl1
      write(out, '(i4, f5.2)') ii, rl1

      read(in, '(i4,f16.5,f6.6, f3.3)') ii, rl3, cx1
      write(out, '(i4, f7.2, 2f8.8)') ii, rl3, cx1

      read(in, '(f8.1, f3.1, f4.1, f3.1, f5.1)') cx2, cx1, rl3
      write(out, '(5f8.2)') cx2, cx1, rl3
      
      read(in, '(f9.1, f4.1, f5.1, f10.1)') rl1, rl2, cx1
      write(out, '(4f10.1)') rl1, rl2, cx1

      read(in, '(f5.4, f8.0, f4.1, f20.1)') cx2, cx3
      write(out, '(4f5.1)') cx2, cx3

      close(in)
      close(out)

      end
