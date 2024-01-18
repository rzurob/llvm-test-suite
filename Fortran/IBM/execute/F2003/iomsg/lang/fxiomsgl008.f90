!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl008.f
! %VERIFY: fort.18:fxiomsgl008.vf
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
!*  TEST CASE TITLE            : Missing scale factor for P format code
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : READ  WRITE  FORMAT
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : The scale specifier is missing for the P format
!*                               code in READ/WRITE I/O statement. The iomsg
!*                               specifier in READ/WRITE was passed in using a
!*                               derived type variable.
!*
!*  TEST CONDITIONS            : 1) Missing scale factor on write with PF fmt cd
!*                               2) Missing scale factor on read with PF fmt cd
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


      program fxiomsgl008
 
      use message_mod

      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id, ios            ! Test Case id under test.
 
      integer*4 varint
 
      character*10 form

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
 
      form = '( PF5.1 )'
 
      write ( 9, fmt = form , iostat = ios, iomsg = t_msg%errmsg) varint

      write(18,*) t_msg%errmsg
 
      rewind 9
 
!
!  TestCase 2...
!
 
      case_id = case_id + 1
 
      form = '( PF5.1 )'
 
      read ( 9, fmt = form , iostat = ios, iomsg = t_msg%errmsg) varint

      write(18,*) t_msg%errmsg

 
!  Clean up...
 
      close ( 9, status = 'DELETE' )
 
      end                            ! End of TestCase.
