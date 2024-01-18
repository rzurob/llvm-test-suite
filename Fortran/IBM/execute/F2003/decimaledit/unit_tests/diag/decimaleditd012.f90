!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditd012.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 08, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This diagnostic test, checks to make sure
!*                               invalid values for the DECIMAL= specifier
!*                               get flagged at run-time for external files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(15) :: val1, val2, val3, val4, val5, val6 ! valid ones
      character(15) :: inval1, inval2, inval3, inval4, inval5 ! invalid
      real :: input

      val1 = 'PoInT'
      val2 = 'pOiNt'
      val3 = 'CoMMa'
      val4 = 'COMMA'
      val5 = 'comma'
      val6 = 'point'

      inval1 = 'pomma'
      inval2 = 'coint'
      inval3 = ''
      inval4 = 'notavalidvalue'
      inval5 = 'commma'

      ! valid values: ( should not be flagged )
      open(unit=77, file='decimaleditd012.dat', decimal=val1)
      open(unit=88, file='tmp.dat')
      read(77, *, decimal=val2) input
      write(88,*,decimal=val3) 3.14
      read(77,*,decimal=val4) input
      write(88,*,decimal=val5) 3.14
      read(77, *, decimal=val6) input
      close(77)

      ! invalid values: ( should be flagged at runtime-time )
      open(unit=77, file='decimaleditd012.dat', decimal=inval1)
      read(77, *, decimal=inval2) input
      write(88,*,decimal=inval3) 3.14
      read(77,*,decimal=inval4) input
      write(88,*,decimal=inval5) 3.14

      end
