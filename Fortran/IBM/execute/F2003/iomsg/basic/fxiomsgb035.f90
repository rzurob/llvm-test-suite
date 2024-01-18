!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb035.f
! %VERIFY: fort.18:fxiomsgb035.vf
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
!*  PRIMARY FUNCTIONS TESTED   : READ  FORMAT
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : An invalid character is put to the right of the
!*                               period in format code with READ.
!*
!*  TEST CONDITIONS            : 1) Invalid character in D fmt code, sequential
!*                               2) Invalid character in B fmt code, direct
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb035

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios            ! Test Case id under test.

      real*4 varreal

      character*20 form

      character*300 errmsg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
!  Open files to read from
!

      open ( 9, access = 'SEQUENTIAL', err = 10 )

      write ( 9, fmt = '( I5 )', err = 10 ) 111

      open ( 8, access = 'DIRECT', form = 'FORMATTED', recl = 20, &
     &  err = 10 )

      write ( 8, err = 10, fmt = '( I5 )', rec = 1 ) 111

      rewind 9

!
! TestCase 1...
!

      case_id = case_id + 1

      form = '( D5.j )'

      read ( 9, fmt = form, iostat = ios, iomsg = errmsg, end = 10 ) varreal

      write(18,*) errmsg

!
! TestCase 2...
!

      case_id = case_id + 1

      form = '( B5._ )'

      read ( 8, fmt = form, iostat = ios, iomsg = errmsg, rec = 1 ) varreal

      write(18,*) errmsg

!  Clean up...

      close ( 9, status = 'DELETE' )

      close ( 8, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.
