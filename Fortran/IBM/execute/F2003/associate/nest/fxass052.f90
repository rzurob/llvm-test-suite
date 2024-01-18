!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass052.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass052.f
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
!*                                     with expressions and with
!*                                     real, real*4, real*8, real*16
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

      program fxass52
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

      assoc: associate ( arg => a )
             assoc2: associate ( arg4 => a4 )
               assoc3: associate ( arg8 => a8 )
                 assoc4: associate ( arg16 => a16 )
                   assoc5: associate ( arg_1 => aa )
                       if (.not.precision_r6(arg_1,aa)) then
                       error stop 6
                       endif
                 end associate assoc5
                       if (.not.precision_r6(arg16,a16)) then
                       error stop 5
                       endif
               end associate assoc4
                       if (.not.precision_r8(arg8,a8)) then
                       error stop 4
                       endif
             end associate assoc3
                       if (.not.precision_r4(arg4,a4)) then
                       error stop 3
                       endif
           end associate assoc2
                       if (.not.precision_r4(arg,a)) then
                       error stop 2
                       endif
          end associate assoc

          assc: associate ( ar => a + (b + 1)*10.0 )
             assc2: associate ( ar4 => a4 + (b4 + 1)*10.0 )
               assc3: associate ( ar8 => a8 + (b8 + 1)*10.0 )
                 assc4: associate ( ar16 => a16 + (b16 + 1)*10.0 )
                   assc5: associate ( ar_1 => aa + (bb + 1)*10.0 )
                       if (.not.precision_r8(ar_1,cc)) then
                       error stop 16
                       endif
                 end associate assc5
                       if (.not.precision_r6(ar16,c16)) then
                       error stop 15
                       endif
               end associate assc4
                       if (.not.precision_r8(ar8,c8)) then
                       error stop 14
                       endif
             end associate assc3
                       if (.not.precision_r4(ar4,c4)) then
                       error stop 13
                       endif
           end associate assc2
                       if (.not.precision_r4(ar,c)) then
                       error stop 12
                       endif
         end associate assc
 end

