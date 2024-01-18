!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb009.f
! %VERIFY: fort.18:fxiomsgb009.vf
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
!*  TEST CASE TITLE            : Invalid unit number with WAIT
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : WAIT
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : The UNIT command has to be in the range 1 
!*                               through 2147483647.
!*                               
!*
!*
!*  TEST CONDITIONS            : 1) WAIT with unit number -9.
!*                              
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb009
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id                 ! Test Case id under test.

      integer*4 a , ios

      character*300 errmsg
 
 
!
!  Unit number too small ( unit = -9 )
!
 
      a = -9
 
!
! TestCase 1...
!
 
      case_id = case_id + 1

      WAIT( ID=a, err=10, iostat = ios, iomsg = errmsg )
 

      call zzrc ( case_id )

10    write(18,*) errmsg

      if ( ios <= 0 ) call zzrc ( case_id )

      end                     ! End of TestCase.


