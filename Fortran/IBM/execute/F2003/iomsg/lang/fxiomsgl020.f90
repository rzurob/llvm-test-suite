!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl020.f
! %VERIFY: fort.18:fxiomsgl020.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f message_mod.mod
! %END
!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : An error is given when an OPEN statement tries
!*                               to open a new file with status old.The iomsg
!*                               specifier was passed in by one element of a
!*                               string array in a derived type which is an
!*                               element of a derived type array.
!*
!*  TEST CONDITIONS            : 1) Open new seq. formatted file as old.
!*                               2) Open new direct unformatted file as old.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      MODULE message_mod

        TYPE message

           integer(4)      useless
           character(300)  errmsg(5)

        END TYPE message

      END MODULE message_mod

      program fxiomsgl020

      use message_mod

      implicit none                     ! All variables must be Declared

      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios

      TYPE ( message )  t_msg(3)

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      open ( 9, status = 'OLD', access = 'SEQUENTIAL', form =  &
  &    'FORMATTED', err = 100, iostat = ios, iomsg = t_msg(2)%errmsg(4) )

      call zzrc ( case_id )

100   write( 18, * ) t_msg(2)%errmsg(4)

      if ( ios <> 6 ) call zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      open ( 9, status = 'OLD', access = 'DIRECT', form = &
   &   'UNFORMATTED', err = 400, iostat = ios, recl = 20 )

      call zzrc ( case_id )

400   write( 18, * ) t_msg(2)%errmsg(4)

      if ( ios <> 6 ) call zzrc ( case_id )

! Clean up....

      close ( 9, status = 'DELETE' )

      end                            ! End of TestCase.
