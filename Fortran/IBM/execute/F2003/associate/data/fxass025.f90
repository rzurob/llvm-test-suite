!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass025.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass025.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
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
!*  DESCRIPTION                : Test: ASSOCIATE
!*                                     with integer, integer*1, integer*2
!*                                     integer*4, integer*8 and byte
!*                                     data types with intrinsic
!*                                     function MOD and do loop.using
!*                                     associate result with expressions.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass25a
      implicit none

      integer a / 1 /

      integer*1 a1 / 2 /
      integer*1 b1 / 8 /

      integer*2 a2 / 5 /
      integer*2 b2 / 4 /
      integer*2 c2

      integer*4 a4 / 9 /

      integer*8 a8 / 9 /

      byte ab1 / 1 /
      byte ab2 / 4 /
      byte cb

!-----------   ASSOCIATE with INTEGER expressions ----------------

      associate ( arg => a )
         arg = 10
      end associate
         if(a .ne. 10)then
           error stop 1
         endif

!-----------   ASSOCIATE with INTEGER*1 expressions ----------------

      associate ( arg1 => a1 )
         arg1 = b1 + 10_1
      end associate
         if(a1 .ne. (b1 + 10_1))then
           error stop 2
         endif

!-----------   ASSOCIATE with INTEGER*2 expressions ----------------

      c2 = (b2 + 1_2)*10_2
      associate ( arg2 => a2 )
         arg2 = (b2 + 1_2)*10_2
      end associate
         if(a2 .ne. c2)then
           error stop 3
         endif

!-----------   ASSOCIATE with INTEGER*4 expressions ----------------

      associate ( arg4 => a4 )
         arg4 = arg4*0
      end associate
         if(a4 .ne. 0)then
           error stop 4
         endif

!-----------   ASSOCIATE with INTEGER*8 expressions ----------------

      associate ( arg8 => a8 )
         arg8 = MOD(9_8,2_8)
      end associate
         if(a8 .ne. MOD(9_8,2_8))then
           error stop 5
         endif


!-----------   ASSOCIATE with BYTE expressions ----------------

      cb = ab1 + (ab2 + 1_1)*10_1
      associate ( arg_1 => ab1 )
         arg_1 = cb
      end associate
         if(ab1 .ne. cb)then
           error stop 6
         endif

      end
