!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 08, 2005
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
!*                               get flagged at compile-time for external files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      ! valid values: ( should not be flagged )
      open(unit=77, file='decimaleditd010.dat', decimal='PoInT')
      read(77,*,decimal='pOiNt')
      write(77,*,decimal='CoMMa')
      close(77)
      open(unit=77, file='decimaledit010.dat', decimal='COMMA')
      read(77,*,decimal='comma')
      write(77,*,decimal='point')
      close(77)

      ! invalid values: ( should be flagged at compile-time )
      open(unit=77, file='decimaledit010.dat', decimal='pomma')
      read(77, *, decimal='coint')
      write(77,*,decimal='')
      read(77, *, decimal='notavalidvalue')

      end
