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
!*  DESCRIPTION                : Test: MULTIPLE NESTED ASSOCIATE
!*                                     with expressions and
!*                                     with complex, complex(4),
!*                                     complex(8,16,32) and double
!*                                     complex data types and do loop.
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

      program fxass78
      implicit none


!-----------   ASSOCIATE with COMPLEX expressions ----------------

      complex a / (1.0e0, 2.0e0) /
      complex b / (3.0e0, 4.0e0) /
      complex c

      complex(4) a4 / (2.0e0, 3.0e0) /
      complex(4) b4 / (3.0e0, 4.0e0) /
      complex(4) c4

      complex*8 a8 / (1.0e0, 2.0e0) /
      complex*8 b8 / (3.0e0, 4.0e0) /
      complex*8 c8

      complex*16 a16 / (1.0d0, 2.0d0) /
      complex*16 b16 / (3.0d0, 4.0d0) /
      complex*16 c16

      complex*32 a32 / (1.0q0, 2.0q0) /
      complex*32 b32 / (3.0q0, 4.0q0) /
      complex*32 c32

      logical :: precision_x3, precision_x6, precision_x8

      c = a + (b + 1)*10
      c4 = a4 + (b4 + 1)*10
      c8 = a8 + (b8 + 1)*10
      c16 = a16 + (b16 + 1)*10
      c32 = a32 + (b32 + 1)*10

           assoc: associate ( arg => a , d => (b + 1)*10 )
             assoc2: associate ( arg4 => a4 , e => (b4 + 1)*10 )
               assoc3: associate ( arg8 => a8 , f => (b8 + 1)*10)
                 assoc4: associate ( arg16 => a16 , g => (b16 + 1)*10)
                   assoc5: associate ( arg32 => a32 , h => (b32 + 1)*10)
                       arg32 = arg32 + h
                       if (.not.precision_x3(arg32,c32)) then
                       error stop 6
                       endif
                    end associate assoc5
                       arg16 = arg16 + g
                       if (.not.precision_x6(arg16,c16)) then
                       error stop 5
                       endif
                  end associate assoc4
                       arg8 = arg8 + f
                       if (.not.precision_x8(arg8,c8)) then
                       error stop 4
                       endif
               end associate assoc3
                       arg4 = arg4 + e
                       if (.not.precision_x8(arg4,c4)) then
                       error stop 3
                       endif
             end associate assoc2
                       arg = arg + d
                       if (.not.precision_x8(arg,c)) then
                       error stop 2
                       endif
         end associate assoc
 end
