!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl010.f
! %VERIFY: fort.18:fxiomsgl010.vf
! %STDIN: 
! %STDOUT:  
! %EXECARGS:
! %POSTCMD: 
! %END
!***************************************************************************
 

!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            : Invalid unit numbers and file types -w/ ENDFILE
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : ENDFILE
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : The UNIT command has to be in the range 1 
!*                               through 2147483647. also I/O statement ENDFILE
!*                               is only for file connected for sequential 
!*                               access, not for direct access. The iomsg 
!*                               specifier in REWIND I/O statement was passed 
!*                               in using a derived type variable.
!*
!*
!*  TEST CONDITIONS            : 1) ENDFILE statement with unit number -9
!*                               2) ENDFILE stmt with direct unformatted file
!*                               3) ENDFILE stmt with direct formatted file
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgl010
 
      implicit none                     ! All variables must be Declared

      
        TYPE message

           integer(4)      useless
           character(300)  errmsg

        END TYPE message


        TYPE message2

           integer          useless2
           TYPE ( message ) errmsg

        END TYPE message2

      integer*4 case_id                 ! Test Case id under test.
      integer*4 a, ios
      TYPE( message2 ) t_msg
 
!
!  Unit number too small ( unit = -9 )
!
 
      a = -9
 
!
! TestCase 1...
!
 
      case_id = case_id + 1
 
      endfile ( unit = a, err = 10, iostat = ios, iomsg = t_msg%errmsg%errmsg )

 
      call zzrc ( case_id )

 10   write(18,*) t_msg%errmsg%errmsg

      if ( ios <> 36 ) call zzrc ( case_id )
 
 
      open ( 9, access = 'DIRECT', recl = 80 )
 
      open ( 8, form = 'FORMATTED', access = 'DIRECT', recl = 80 )
 
!
! TestCase 2...
!
 
      case_id = case_id + 1
 
      endfile ( 9, err = 20, iostat =ios, iomsg = t_msg%errmsg%errmsg )
 

      call zzrc ( case_id )

 20   write(18,*) t_msg%errmsg%errmsg

      if ( ios <> 17 ) call zzrc ( case_id )
 
!
! TestCase 3...
!
 
      case_id = case_id + 1
 
      endfile ( 9, err = 30, iostat =ios, iomsg = t_msg%errmsg%errmsg )
 

      call zzrc ( case_id )

 30   write(18,*) t_msg%errmsg%errmsg

      if ( ios <> 17 ) call zzrc ( case_id )
 
! Clean up...
 
      close ( 8, status = 'DELETE' )
 
      close ( 9, status = 'DELETE' )
 
      end                     ! End of TestCase.
