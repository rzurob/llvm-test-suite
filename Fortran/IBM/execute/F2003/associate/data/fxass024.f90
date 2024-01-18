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
! %GROUP: fxass024.f
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
!*  TEST CASE NAME             : fxass024.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE, different data type
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
!*  DESCRIPTION                : Test: ASSOCIATE with expressions and
!*                                     with integer constant array
!*                                     and do loop. test with intrinsic
!*                                     function real.
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

   program fxass24a
   implicit none
  
   integer i
   integer,PARAMETER :: a(50) = (/(i,i=1,50)/)
  
   do i = 1,50 
      associate ( arg => a(i) + 10 )
      if ( arg .ne. (i+10) ) error stop 1
      end associate
   end do

   !-------------------------------
   !  Test real
   !-------------------------------

   do i = 1,50 
      associate ( ar => real(a(i) / 2) )
      if ( ar .ne. real(i/2) ) error stop 11
      end associate
   end do

   !-------------------------------
   !  Test complex
   !-------------------------------

   do i = 1,50 
      associate ( ac16 => (real(a(i)),REAL(a(i))) + (1.0,1.0))
      if ( ac16 .ne. ((real(i),REAL(i)) + (1.0,1.0))) error stop 17
      end associate
   end do 

   loop1: do i = 1,50 
      associate ( ac32 => (real(a(i)),REAL(a(i))) )
      if ( ac32 .ne. (real(i),REAL(i)) ) error stop 18
      end associate
   end do loop1

   end
