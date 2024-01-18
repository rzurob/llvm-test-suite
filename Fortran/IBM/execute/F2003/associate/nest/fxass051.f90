!**********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Test: NESTED ASSOCIATE,associate result
!*                                     with expressions and with
!*                                     integer, integer*1, integer*2
!*                                     integer*4, integer*8 and byte
!*                                     data types.
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

      program fxass051
      implicit none

      integer a / 1 /
      integer b / 4 /
      integer c

      integer*1 a1 / 2 /
      integer*1 b1 / 8 /
      integer*1 c1

      integer*2 a2 / 5 /
      integer*2 b2 / 4 /
      integer*2 c2

      integer*4 a4 / 9 /
      integer*4 b4 / 2 /
      integer*4 c4

      integer*8 a8 / 9 /
      integer*8 b8 / 2 /
      integer*8 c8
      byte ab1 / 1 /
      byte ab2 / 4 /
      byte cb

!-----------   ASSOCIATE with INTEGER expressions ----------------
      c = a + (b + 1)*10
      c1 = a1 + (b1 + 1)*10
      c2 = a2 + (b2 + 1)*10
      c4 = a4 + (b4 + 1)*10
      c8 = a8 + (b8 + 1)*10
      cb = ab1 + (ab2 + 1)*10

      assoc: associate ( arg => a )
       arg = arg + (b + 1)*10
        assoc1: associate ( arg1 => a1 )
         arg1 = arg1 + (b1 + 1)*10
          assoc2: associate ( arg2 => a2 )
           arg2 = arg2 + (b2 + 1)*10
            assoc3: associate ( arg4 => a4 )
             arg4 = arg4 + (b4 + 1)*10
              assoc4: associate ( arg8 => a8 )
               arg8 = arg8 + (b8 + 1)*10
                assoc5: associate ( arg_1 => ab1 )
                 arg_1 = arg_1 + (ab2 + 1)*10
                       if(arg_1 .ne. cb)then
                       error stop 6
                       endif
                 end associate assoc5
                       if(arg8 .ne. c8)then
                       error stop 5
                       endif
               end associate assoc4
                       if(arg4 .ne. c4)then
                       error stop 4
                       endif
             end associate assoc3
                       if(arg2 .ne. c2)then
                       error stop 3
                       endif
           end associate assoc2
                       if(arg1 .ne. c1)then
                       error stop 2
                       endif
         end associate assoc1
                       if(arg .ne. c)then
                       error stop 1
                       endif
      end associate assoc
 end
