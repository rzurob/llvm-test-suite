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
! %COMPOPTS: -qfree=f90 -qrealsize=8
! %GROUP: fxass088.f
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
!*  TEST CASE NAME             : fxass088.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
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
!*  DESCRIPTION                : Test: ASSOCIATE with expressions 
!*                                     with real data type and using 
!*                                     different -qrealsize options.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass88
      implicit none

      real a / 1.9 /
      real b / 4.4 /

      real a4 / 9.0 /
      real b4 / 2.98 /

      real a8 / 9.0 /
      real b8 / 2.09 /
      
      real a16 / 9.9 /
      real b16 / 2.0 /
      real c16

      logical :: precision_r8

      integer count

!-----------   ASSOCIATE with REAL expressions ----------------

      associate ( arg => (a + b)*10 + 10.0 )
         if (.not.precision_r8(arg,((a + b)*10 + 10.0))) then
           error stop 1
         endif
      end associate


!-----------   ASSOCIATE with REAL expressions ----------------

      associate ( arg4 => 1.0 )
         if (.not.precision_r8(arg4,1.0)) then
           error stop 4
         endif
      end associate


!-----------   ASSOCIATE with REAL expressions ----------------

      associate ( arg8 => a8 )
         arg8 = arg8 + b8
         if (.not.precision_r8(arg8,a8)) then
           error stop 5
         endif
      end associate


!-----------   ASSOCIATE with REAL*16 expressions ----------------

      c16 = (a16 + b16)*10.0 + 1.0   

      associate ( arg16 => (a16 + b16)*10.0 + 1.0 )
         if (.not.precision_r8(arg16,c16)) then
           error stop 6
         endif
      end associate


      end
