!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : NESTED ASSOCIATE with -qrealsize option
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

      program fxass91
      implicit none

      real a / 1.9 /
      real b / 4.4 /
      real c,c1

      real a4 / 9.0 /
      real b4 / 2.98 /

      logical :: precision_r4

!-----------   ASSOCIATE with REAL expressions ----------------

      assoc: associate ( arg => a )
             assoc2: associate ( arg4 => a4 )
                if (.not.precision_r4(arg4,a4)) then
                 error stop 3
                endif
             end associate assoc2
                if (.not.precision_r4(arg,a)) then
                 error stop 2
                endif
      end associate assoc
          c = a + (b + 1.0)*10.0
          c1 = a4 + (b4 + 1.0)*10.0
          assc: associate ( ar => a + (b + 1.0)*10.0 )
             assc2: associate ( ar4 => a4 + (b4 + 1.0)*10.0 )
                if (.not.precision_r4(ar4,(a4 + (b4 + 1.0)*10.0))) then
                 error stop 13
                endif
             end associate assc2
                if (.not.precision_r4(ar,(a + (b + 1.0)*10.0))) then
                 error stop 12
                endif
         end associate assc
 end

