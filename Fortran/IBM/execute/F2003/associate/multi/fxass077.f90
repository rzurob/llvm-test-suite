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
!*  DESCRIPTION                : Test: MULTIPLE NESTED ASSOCIATE
!*                                     with expressions and
!*                                     with real, real*4, real*8, real*16
!*                                     double precision data types with
!*                                     do loop.
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

      program fxass77
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


      c = a + (b + 1)*10.0
      c4 = a4 + (b4 + 1)*10.0
      c8 = a8 + (b8 + 1)*10.0
      c16 = a16 + (b16 + 1)*10.0
      cc = aa + (bb + 1)*10.0

!-----------   ASSOCIATE with REAL expressions ----------------

          assc: associate ( ar => a + (b + 1)*10.0 , i => c )
             assc2: associate ( ar4 => a4 + (b4 + 1)*10.0 , j => c4)
               assc3: associate ( ar8 => a8 + (b8 + 1)*10.0 , k => c8)
                 assc4: associate ( ar16 => a16 + (b16 + 1)*10.0 , l => c16)
                   assc5: associate ( ar_1 => aa + (bb + 1_8)*10.0_8 , m => cc)
                       if (.not.precision_r8(ar_1,m)) then
                       error stop 16
                       endif
                 end associate assc5
                       if (.not.precision_r6(ar16,l)) then
                       error stop 15
                       endif
               end associate assc4
                       if (.not.precision_r8(ar8,k)) then
                       error stop 14
                       endif
             end associate assc3
                       if (.not.precision_r4(ar4,j)) then
                       error stop 13
                       endif
           end associate assc2
                       if (.not.precision_r4(ar,i)) then
                       error stop 12
                       endif
         end associate assc
 end

