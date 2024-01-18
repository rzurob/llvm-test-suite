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
! %COMPOPTS: -qfree=f90 -qintsize=1
! %GROUP: fxass090.f
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
!*  TEST CASE NAME             : fxass090.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : iNESTED ASSOCIATE with -qintsize option
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

      program fxass090
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

      byte ab1 / 1 /
      byte ab2 / 4 /
      byte cb

      c = a + (b + 1)*10 
      c1 = a1 + (b1 + 1)*10
      c2 = a2 + (b2 + 1)*10
      c4 = a4 + (b4 + 1)*10
      c8 = a8 + (b8 + 1)*10
      cb = ab1 + (ab2 + 1)*10

!-----------   ASSOCIATE with INTEGER expressions ----------------

      assoc: associate ( arg => a + (b + 1_1)*10_1 )
        assoc1: associate ( arg1 => a1 + (b1 + 1_1)*10_1 )
           assoc2: associate ( arg2 => a2 + (b2 + 1_1)*10_1 )
             assoc3: associate ( arg4 => a4 + (b4 + 1_1)*10_1 )
               assoc4: associate ( arg8 => a8 + (b8 + 1_1)*10_1 )
                 assoc5: associate ( arg_1 => ab1 + (ab2 + 1_1)*10_1 )
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

      assc: associate ( ar => a )
        assc1: associate ( ar1 => a1 )
           assc2: associate ( ar2 => a2 )
             assc3: associate ( ar4 => a4 )
               assc4: associate ( ar8 => a8 )
                 assc5: associate ( ar_1 => ab1 )
                       if(ar_1 .ne. ab1)then
                       error stop 16
                       endif
                 end associate assc5
                       if(ar8 .ne. a8)then
                       error stop 15
                       endif
               end associate assc4
                       if(ar4 .ne. a4)then
                       error stop 14
                       endif
             end associate assc3
                       if(ar2 .ne. a2)then
                       error stop 13
                       endif
           end associate assc2
                       if(ar1 .ne. a1)then
                       error stop 12
                       endif
         end associate assc1
                       if(ar .ne. a)then
                       error stop 11
                       endif
      end associate assc

 end
