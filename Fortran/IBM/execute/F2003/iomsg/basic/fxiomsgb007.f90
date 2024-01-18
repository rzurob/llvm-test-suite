!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb007.f
! %VERIFY: fort.18:fxiomsgb007.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WRITE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : Unformatted write attempt on formatted file or
!*                               Formatted write attempt on unformatted file are
!*                               all not allowed.
!*
!*  TEST CONDITIONS            : 1) Unformatted write attempt on formatted file
!*                             : 2) Formatted write attempt on unformatted file
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb007

      implicit none

      integer*4  case_id, ios

      integer*4 ivar

      character*300 errmsg

!
!  Initialize Return Code routine to 0...
!
      case_id = 0
      call zzrc (case_id )

!
!  Test case 1...
!

      ivar = 125

      write( 10, '(I4)', err = 100, iostat = ios ) ivar

      write( 10, err = 100, iostat = ios, iomsg = errmsg ) ivar

      goto 300

100   write(18,*) errmsg

!
!  Test case 2...
!
      ivar = 225

      write( 11, err = 200, iostat = ios ) ivar

      write( 11, '(I4)', err = 200, iostat = ios, iomsg = errmsg ) ivar

      goto 300

200   write(18,*) errmsg

! Clean up....

300   close ( 10, status = 'DELETE' )

      close ( 11, status = 'DELETE' )

      end

