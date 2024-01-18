!**********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!**********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; 
! %COMPOPTS: -qfree=f90 -qrealsize=4
! %GROUP: fxass093.f
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
!*  TEST CASE NAME             : fxass093.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : MULTIPLE ASSOCIATE with -qreasize option
!*  KEYWORD(S)                 : ASSOCIATE, real
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
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass93
      implicit none

      real a / 1.9 /
      real b / 4.4 /
      real c

      real a4 / 9.0 /
      real b4 / 2.98 /
      real c4

      real a8 / 9.0d0 /
      real b8 / 2.09 /
      real c8
      logical :: precision_r4
      
!-----------   ASSOCIATE with REAL expressions ----------------

      c = a + (b + 1)*10
      associate ( arg => a , arg1 => b , arg2 => c )
         arg = arg + (arg1 + 1)*10
           if (.not.precision_r4(arg,arg2)) then
           error stop 1
           endif
      end associate

!-----------   ASSOCIATE with REAL*4 expressions ----------------

      c4 = a4 + (b4 + 1)*10
      associate ( ar => a4 , ar1 => b4 , ar2 => c4 )
         ar = ar + (ar1 + 1)*10
           if (.not.precision_r4(ar,ar2)) then
           error stop 4
           endif
      end associate

!-----------   ASSOCIATE with REAL*8 expressions ----------------

      c8 = a8 + (b8 + 1)*10
      associate ( ag => a8 , ag1 => b8 , ag2 => c8 )
         ag = ag + (ag1 + 1)*10
           if (.not.precision_r4(ag,ag2)) then
           error stop 5
           endif
      end associate

      end
