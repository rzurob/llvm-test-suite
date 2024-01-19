!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
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
!*  DESCRIPTION                : Test: NESTED ASSOCIATE
!*                                     associat result with expressions and
!*                                     with real, real*4, real*8, real*16
!*                                     double precision data types.
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

      program fxass53
      implicit none

      real a / 1.9 /
      real b / 4.4 /
      real c

      real*4 a4 / 9.0 /
      real*4 b4 / 2.98 /
      real*4 c4

      real*8 a8 / 9.0d0 /
      real*8 b8 / 2.09 /
      real*8 c8

      real*16 a16 / 9.9 /
      real*16 b16 / 2.0q0 /
      real*16 c16

      double precision aa / 1.0d0 /
      double precision bb / 4.0d0 /
      double precision cc

      logical :: precision_r4, precision_r6, precision_r8

!-----------   ASSOCIATE with REAL expressions ----------------

      c = a + (b + 1)*10
      c4 = a4 + (b4 + 1)*10
      c8 = a8 + (b8 + 1)*10
      c16 = a16 + (b16 + 1)*10
      cc = aa + (bb + 1)*10

      assoc: associate ( arg => a )
             assoc2: associate ( arg4 => a4 )
               assoc3: associate ( arg8 => a8 )
                 assoc4: associate ( arg16 => a16 )
                   assoc5: associate ( arg_1 => aa )
                     arg_1 = arg_1 + (bb + 1_8)*10_8
                       if (.not.precision_r8(arg_1,cc)) then
                       error stop 6
                       endif
                 end associate assoc5
                       arg16 = arg16 + (b16 + 1)*10
                       if (.not.precision_r6(arg16,c16)) then
                       error stop 5
                       endif
               end associate assoc4
                       arg8 = arg8 + (b8 + 1)*10
                       if (.not.precision_r8(arg8,c8)) then
                       error stop 4
                       endif
             end associate assoc3
                       arg4 = arg4 + (b4 + 1)*10
                       if (.not.precision_r4(arg4,c4)) then
                       error stop 3
                       endif
           end associate assoc2
                       arg = arg + (b + 1)*10
                       if (.not.precision_r4(arg,c)) then
                       error stop 2
                       endif
         end associate assoc
 end

