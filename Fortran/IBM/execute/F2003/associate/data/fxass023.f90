!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
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
!*  DEPendENCIES               : External routine ZZRC
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

   program fxass23a
   implicit none

   integer i
   integer,PARAMETER :: a(50) = (/(i,i=1,50)/)

   !-------------------------------
   !  Test integer
   !-------------------------------

   do  i = 1,50
      associate ( arg => a(i) )
      if ( arg .ne. i ) error stop 1
      end associate
   end do

   !-------------------------------
   !  Test real
   !-------------------------------

   do i = 1,50
      associate ( ar => real(a(i)) )
      if ( ar .ne. real(i) ) error stop 11
      end associate
   end do

   !-------------------------------
   !  Test complex
   !-------------------------------

   do i = 1,50
      associate ( ac => (real(a(i)),REAL(a(i))) )
      if ( ac .ne. ((real(i),REAL(i))) ) error stop 15
      end associate
   end do

   end
