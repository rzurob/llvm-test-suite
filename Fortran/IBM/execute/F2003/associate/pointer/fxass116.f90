!**********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!**********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90
! %GROUP: fxass116.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass116.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,real
!*  TARGET(S)                  :
!*  NUMBER OF TESTS            : 1
!*  STATUS                     : done
!*
!*  STRUCTURE                  : Main program
!*  EXECUTABLE                 : Yes
!*
!*  INPUTS                     : None
!*  OUTPUTS                    : None
!*
!*  SETUP REQUIREMENTS         : N/A
!*  DEPENDENCIES               : External routine ZZRC
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  CONDITIONS TESTED          : Listed below.
!*
!*  DESCRIPTION                : Test: ASSOCIATE with POINTER
!*                                     with real, real*4, real*8
!*                                     real*16 data types.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass116
      implicit none

      real, target :: a = 2.0
      real, pointer :: b 

      real*4, target :: a4 = 8.5
      real*4, pointer :: b4 

      real*8, target :: a8 = 10.10 
      real*8, pointer :: b8 

      real*16, target :: a16 = 10.3 
      real*16, pointer :: b16 

      logical :: precision_r4, precision_r6, precision_r8

      b => a
      b4 => a4
      b8 => a8
      b16 => a16 

      associate ( arg => b )
      if (.not.precision_r4(arg,a)) then
         error stop 1
      endif
      end associate
  
      associate ( arg3 => (b4 * 3) + 2, arg4 => b8 )
      if (.not.precision_r4(arg3,((a4 * 3) + 2))) then
         error stop 4
      endif

      if (.not.precision_r8(arg4,a8)) then
         error stop 5
      endif
      end associate

      associate ( arg5 => b16 )
      if (.not.precision_r6(arg5,a16)) then
         error stop 6
      endif
      end associate

      end
      
