!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb025.f
! %VERIFY: fort.18:fxiomsgb025.vf
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
!*  PRIMARY FUNCTIONS TESTED   : READ
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : Reads data from an internal file that is of a
!*                               different type than the variable use to write
!*                               it.
!*
!*  TEST CONDITIONS            : 1) List-dir read real with logical variable.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb025

      implicit none

      integer*4       case_id
      integer*4       ios
      character*30    unit2
      character*300   errmsg
      logical*4       lrvar
      real            var11

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! DEFINE values
!

      var11 = 3.15142669

!
! WRITE to test files
!

      write ( unit = unit2, fmt = * )  var11

!
! TestCase 1...
!

      case_id = case_id + 1

      read ( unit = unit2, fmt = *, iostat = ios, iomsg=errmsg ) lrvar

      write(18 ,*) errmsg

      if ( ios <> 42 ) call zzrc ( case_id )

      end                            ! End of TestCase.
