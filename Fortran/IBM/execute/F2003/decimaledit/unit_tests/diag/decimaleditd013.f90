!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditd013.f
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
!*                               get flagged at run-time for internal files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(15) :: val1, val2, val3, val4, val5, val6 ! valid ones
      character(15) :: inval1, inval2, inval3, inval4, inval5 ! invalid
      real :: input
      character(20) :: buffer

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
      write(buffer, *, decimal=val1) 3.14
      read(buffer, *, decimal=val2) input
      write(buffer,*,decimal=val3) 3.14
      read(buffer,*,decimal=val4) input
      write(buffer,*,decimal=val5) 3.14
      read(buffer, *, decimal=val6) input

      ! invalid values: ( should be flagged at compile-time )
      write(buffer, *, decimal=inval1) 3.14
      read(buffer, *, decimal=inval2) input
      write(buffer,*,decimal=inval3) 3.14
      read(buffer,*,decimal=inval4) input
      write(buffer,*,decimal=inval5) 3.14

      end
