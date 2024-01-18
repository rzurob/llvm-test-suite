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
! %GROUP: fxass020.f
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
!*  TEST CASE NAME             : fxass020.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : ASSOCIATE with LOGICAL expressions
!*                               with nested named do loop and do while
!*  KEYWORD(S)                 : ASSOCIATE,logical
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

      program fxass20a
      implicit none

      logical(4), parameter :: T = .true. 
      logical(8), parameter :: F = .false.

      logical a / .true.  /
      logical b / .false. /
      logical c

      logical*1 a1 / .true. /
      logical*1 b1 / .false. /
      logical*1 c1

      logical*2 a2 / .true. /
      logical*2 b2 / .false. /
      logical*2 c2

      logical*4 a4 / .true. /
      logical*4 b4 / .false. /

      logical*8 a8 / .true. /
      logical*8 b8 / .false. /
      
      integer count,k

!-----------   ASSOCIATE with LOGICAL expressions ----------------
      k = 1
      c = .false.   
      do count = 1, 10
        do while ( K .le. 5 ) 
      associate ( arg => a .eqv. b )
         if(arg .neqv. c)then 
           error stop 1
         endif
      end associate
        k = k + 1
        end do
      end do

!-----------   ASSOCIATE with LOGICAL*1 expressions ----------------

      c1 = .true.   
      do 15 count = 1, 10
       first: do while ( K .le. 5 )
      associate ( arg1 => a1 .neqv. b1 )
         if(arg1 .neqv. c1)then
           error stop 2
         endif
      end associate
        
      k = k + 1
      end do first
 15   end do


!-----------   ASSOCIATE with LOGICAL*2 expressions ----------------

      c2 = .false.   
      do count = 1, 10
         do k = 1, 5
      associate ( arg2 => a2 .eqv. b2 )
         if(arg2 .neqv. c2)then
           error stop 3
         endif
      end associate
         end do
      end do

!-----------   ASSOCIATE with LOGICAL*4 expressions ----------------

      loop1: do count = 1, 10
      loop2: do k = 1,5

      associate ( arg4 => a4 .neqv. b4 )
         if(arg4 .neqv. T)then
           error stop 4
         endif
      end associate

      end do loop2
      end do loop1

!-----------   ASSOCIATE with LOGICAL*8 expressions ----------------

      do count = 1, 10

      associate ( arg8 => a8 .eqv. b8 )
         if(arg8 .neqv. F)then
           error stop 5
         endif
      end associate

      end do

      end
