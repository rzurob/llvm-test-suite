!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb047.f
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
!*  TEST CASE TITLE            : ENDFILE with No Erro condition
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
!*                               iomsg specifier remains unchanged.
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

      program fxiomsgb047
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id                 ! Test Case id under test.

      integer*4 ios

      character*300 errmsg
 

 
      open ( 8, access = 'SEQUENTIAL', err=10 )


!
! TestCase 1...
!
 
      case_id = case_id + 1

      errmsg = 'abc'
 
      endfile ( 8, iostat =ios, iomsg = errmsg )
 
      if ( errmsg <> 'abc' ) call zzrc ( case_id )
 
 
! Clean up...
 
      close ( 8, status = 'DELETE' )

      stop ' '

10    call zzrc ( case_id + 100 )
 
      end                     ! End of TestCase.
