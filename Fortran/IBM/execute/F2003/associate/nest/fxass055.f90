!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE, complex
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
!*                                     with expressions and
!*                                     with complex, complex(4),
!*                                     complex(8,16,32) and double
!*                                     complex data types.
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

      program fxass55
      implicit none

      complex a / (1.0e0, 2.0e0) /

      complex(4) a4 / (2.0e0, 3.0e0) /

      complex*8 a8 / (1.0e0, 2.0e0) /

      complex*16 a16 / (1.0d0, 2.0d0) /

      complex*32 a32 / (1.0q0, 2.0q0) /

      double complex aa / (1.0d0, 2.0d0) /

      logical :: precision_x3, precision_x6, precision_x8

!-----------   ASSOCIATE with COMPLEX expressions ----------------


           assoc: associate ( arg => a )
             assoc2: associate ( arg4 => a4 )
               assoc3: associate ( arg8 => a8 )
                 assoc4: associate ( arg16 => a16 )
                   assoc5: associate ( arg32 => a32 )
                       if (.not.precision_x3(arg32,a32)) then
                       error stop 6
                       endif
                 end associate assoc5
                       if (.not.precision_x3(arg16,a16)) then
                       error stop 5
                       endif
               end associate assoc4
                       if (.not.precision_x6(arg8,a8)) then
                       error stop 4
                       endif
             end associate assoc3
                       if (.not.precision_x8(arg4,a4)) then
                       error stop 3
                       endif
           end associate assoc2
                       if (.not.precision_x8(arg,a)) then
                       error stop 2
                       endif
         end associate assoc
 end
