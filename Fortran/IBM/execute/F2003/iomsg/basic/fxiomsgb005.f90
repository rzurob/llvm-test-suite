!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb005.f
! %VERIFY: fort.18:fxiomsgb005.vf
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
!*  PRIMARY FUNCTIONS TESTED   : READ  WRITE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : Create direct formatted file, then read/write
!*                               with REC = 0
!*
!*  TEST CONDITIONS            : 1) Write direct formatted file with rec = 0
!*                               2) Read  direct formatted file with rec = 0
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb005

      implicit none

      integer*4 case_id

      integer*4 ios

      integer*4 rec_num

      character*10 varchar

      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc(case_id)

!
! Create file
!

      rec_num = 0

      open ( 10 , access = 'DIRECT', recl = 6, form = 'FORMATTED' )

!
! TestCase 1...
!

      case_id = case_id + 1

      write ( 10, fmt = '( A )', iostat = ios, iomsg = errmsg, &
  &   rec = rec_num ) 'xXxXxX'

      write ( 18, * ) errmsg

!
! TestCase 2...
!

      read ( 10, fmt = '( A )', iostat = ios, iomsg = errmsg,rec =  &
  &   rec_num ) varchar

      write ( 18, * ) errmsg

100   end

