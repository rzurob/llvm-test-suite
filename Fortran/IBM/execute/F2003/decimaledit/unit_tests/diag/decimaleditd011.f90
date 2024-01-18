!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditd011.f
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
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : This diagnostic test, checks to make sure
!*                               invalid values for the DECIMAL= specifier
!*                               get flagged at compile-time for internal files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(20) :: buffer

      ! valid values: ( should not be flagged )
      read(buffer,*,decimal='PoInT')
      read(buffer,*,decimal='pOiNt')
      write(buffer,*,decimal='CoMMa')
      write(buffer,*,decimal='COMMA')
      read(buffer,*,decimal='comma')
      write(buffer,*,decimal='point')

      ! invalid values: ( should be flagged at compile-time )
      write(buffer,*,decimal='pomma')
      read(buffer, *, decimal='coint')
      write(buffer,*,decimal='')
      read(buffer, *, decimal='notavalidvalue')
      write(buffer, *,decimal='commma')

      end
