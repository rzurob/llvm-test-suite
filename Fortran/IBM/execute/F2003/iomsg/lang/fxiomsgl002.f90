!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl002.f
! %VERIFY:
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
!*  TEST CASE TITLE            : ENDFILE with No Error condition
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
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : ENDFILE with No Error condition to check if the
!*                               iomsg specifier remains unchanged. The iomsg
!*                               specifier was passed in by derived type.
!*                               
!*
!*  TEST CONDITIONS            : 1) ENDFILE statement with sequential file
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgl002
 
      implicit none                     ! All variables must be Declared
 
     
      TYPE message

         integer(4)      useless
         character(300)  errmsg

      END TYPE message
 

      integer*4 case_id                 ! Test Case id under test.
      integer*4 ios
      TYPE ( message )  t_msg
 
      open ( 8, access = 'SEQUENTIAL', err=10 )

!
! TestCase 1...
!
 
      case_id = case_id + 1

      t_msg%errmsg = 'abc'
 
      endfile ( 8, iostat =ios, iomsg = t_msg%errmsg )
 
      if ( t_msg%errmsg <> 'abc' ) call zzrc ( case_id )
 
 
! Clean up...
 
      close ( 8, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )
 
      end                     ! End of TestCase.
