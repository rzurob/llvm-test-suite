!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl007.f
! %VERIFY: fort.18:fxiomsgl007.vf
! %STDIN: 
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f message_mod.mod
! %END
!***************************************************************************


!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            : OPEN with status OLD on NEW file.
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : OPEN
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : An error is given when an OPEN statement tries
!*                               to open a new file with status old. The iomsg
!*                               specifier in OPEN statement was passed in using
!*                               a derived type variable.
!*
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
           character(300)  errmsg

        END TYPE message

      END MODULE message_mod


      program fxiomsgl007

      use message_mod
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id                 ! Test Case id under test.
 
      integer*4 ios

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

      open ( 9, status = 'OLD', access = 'SEQUENTIAL', form =  &
  &    'FORMATTED', err = 100, iostat = ios, iomsg = t_msg%errmsg )
 
      call zzrc ( case_id )
 
100   write( 18, * ) t_msg%errmsg
      if ( ios <> 6 ) call zzrc ( case_id )
 
 
!
! TestCase 2...
!
 
      case_id = case_id + 1

      open ( 9, status = 'OLD', access = 'DIRECT', form = &
   &   'UNFORMATTED', err = 400, iostat = ios, recl = 20 )
 
      call zzrc ( case_id )
 
400   write( 18, * ) t_msg%errmsg
      if ( ios <> 6 ) call zzrc ( case_id )
      
 
! Clean up....
 
      close ( 9, status = 'DELETE' )
 
      end                            ! End of TestCase.
