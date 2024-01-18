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
! %COMPOPTS: -qfree=f90 -qintsize=8
! %GROUP: fxass092.f
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
!*  TEST CASE NAME             : fxass092.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : MULTIPLE ASSOCIATE with -qintsize options 
!*  KEYWORD(S)                 : ASSOCIATE,integer,byte
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

      program fxass92a
      implicit none

      integer a / 1 /
      integer b / 4 /
      integer c

      integer a1 / 2 /
      integer b1 / 8 /
      integer c1

      integer a2 / 5 /
      integer b2 / 4 /
      integer c2

      integer a4 / 9 /
      integer b4 / 2 /
      integer c4

      integer a8 / 9 /
      integer b8 / 2 /
      integer c8
      
!-----------   ASSOCIATE with INTEGER expressions ----------------

      c = a + (b + 1)*10   
      associate ( arg => a , arg1 => b , arg2 => c)
         arg = arg + (arg1 + 1)*10  
         if(arg .ne. arg2)then 
           error stop 1_4
         endif
      end associate

!-----------   ASSOCIATE with INTEGER*1 expressions ----------------

      c1 = a1 + (b1 + 1)*10
      associate ( ar => a1 , ar1 => b1 , ar2 => c1)
         ar = ar + (ar1 + 1)*10
         if(ar .ne. ar2)then
           error stop 2_4
         endif
      end associate


!-----------   ASSOCIATE with INTEGER*2 expressions ----------------

      c2 = a2 + (b2 + 1)*10 
      associate ( ag => a2 , ag1 => b2 , ag2 => c2)
         ag = ag + (ag1 + 1)*10
         if(ag .ne. ag2)then
           error stop 3_4
         endif
      end associate

!-----------   ASSOCIATE with INTEGER*4 expressions ----------------

      c4 = a4 + (b4 + 1)*10   
      associate ( rg => a4 , rg1 => b4 , rg2 => c4)
         rg = rg + (rg1 + 1)*10
         if(rg .ne. rg2)then
           error stop 4_4
         endif
      end associate

!-----------   ASSOCIATE with INTEGER*8 expressions ----------------

      c8 = a8 + (b8 + 1)*10
      associate ( argo => a8 , argo1 => b8 , argo2 => c8)
         argo = argo + (argo1 + 1)*10
         if(argo .ne. argo2)then
           error stop 5_4
         endif
      end associate

      end
