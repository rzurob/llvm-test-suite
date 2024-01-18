!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl001.f
! %VERIFY:
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
!*  DESCRIPTION                : Open files with No Error conditions to check
!*                               if the iomsg specifier remains unchanged.
!*                               This test case passes a derived type to IOMSG
!*                               in OPEN statement.
!*
!*  TEST CONDITIONS            : 1) OPEN formatted file with direct access
!*                               2) OPEN unformatted file with sequential access
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      MODULE message_mod

        TYPE message
           integer(4)      useless
           character(300)  errmsg
        END TYPE message

      END MODULE message_mod

      program fxiomsgl001

      use message_mod

      implicit none                     ! All variables must be Declared

      integer*4 case_id, ios

      TYPE ( message )  t_msg

!
! Initialize Return Code routine to SUCCESS...
!

      case_id = 0
      call zzrc ( case_id )

!
! TestCase 1...
!

      case_id = case_id + 1

      t_msg%errmsg = 'abc'

      open ( 8, access = 'DIRECT', recl = 10, err = 10, &
   &  form = 'FORMATTED', iostat = ios, iomsg = t_msg%errmsg )

      if ( t_msg%errmsg <> 'abc' )  call  zzrc ( case_id )

!
! TestCase 2...
!

      case_id = case_id + 1

      t_msg%errmsg = 'abc'

      open ( 9, access = 'SEQUENTIAL', err = 10, &
     &  iostat = ios, iomsg = t_msg%errmsg )

      if ( t_msg%errmsg <> 'abc' )  call  zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )

      end                            ! End of TestCase.

