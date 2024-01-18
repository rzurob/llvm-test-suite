!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl018.f
! %VERIFY:
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
!*  TEST CASE TITLE            : WRITE with No Error conditions
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : WRITE
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : WRITE with NO Error conditions to check if
!*                               the iomsg specifier remains unchanged. The
!*                               iomsg specifier was passed in by a derived
!*                               type variable which is a member of another
!*                               derived type.
!*
!*  TEST CONDITIONS            : 1) WRITE to sequential file with No Error Cond.
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

        
        TYPE message2

           integer          useless2
           TYPE ( message ) errmsg

        END TYPE message2


      END MODULE message_mod

      program fxiomsgl018

      use message_mod
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id                 ! Test Case id under test.
      integer*4 ios
      TYPE ( message2 )  t_msg(5)
 
!
! Initialize Return Code routine to SUCCESS...
!
 
      case_id = 0
      call zzrc ( case_id )
 
!
!  Test case 1...
!

      case_id = case_id + 1

      t_msg(4)%errmsg%errmsg = 'abc'
 
      open ( 9, access = 'SEQUENTIAL', err = 10 )
 
      write ( 9, fmt = '( A5 )', iostat = ios, iomsg =  &
  &   t_msg(4)%errmsg%errmsg ) 'XXXXX'

      if ( t_msg(4)%errmsg%errmsg <> 'abc' ) call zzrc ( case_id )     
 
 
! Clean up...
 
      close ( 9, status = 'DELETE' )
 
      stop ' '
 
10    call zzrc ( case_id + 100 )
 
      end                            ! End of TestCase.
