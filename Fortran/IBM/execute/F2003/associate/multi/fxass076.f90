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
! %GROUP: fxass076.f
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
!*  TEST CASE NAME             : fxass076.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
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
!*  DESCRIPTION                : Test: MULTIPLE NESTED ASSOCIATE 
!*                                     with expressions and with
!*                                     integer, integer*1, integer*2
!*                                     integer*4, integer*8 and byte
!*                                     data types and do loop.
!*
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

      program fxass076
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

      c = a + (b + 1)*10 
      c1 = a1 + (b1 + 1)*10
      c2 = a2 + (b2 + 1)*10
      c4 = a4 + (b4 + 1)*10
      c8 = a8 + (b8 + 1_8)*10_8
      cb = ab1 + (ab2 + 1)*10

!-----------   ASSOCIATE with INTEGER expressions ----------------

      assoc: associate ( arg => a + (b + 1)*10 , argc => c)
        assoc1: associate ( arg1 => a1 + (b1 + 1_1)*10_1 , argc1 => c1)
           assoc2: associate ( arg2 => a2 + (b2 + 1_2)*10_2 , argc2 => c2)
             assoc3: associate ( arg4 => a4 + (b4 + 1)*10 , argc4 => c4)
               assoc4: associate ( arg8 => a8 + (b8 + 1)*10 , argc8 => c8)
                 assoc5: associate ( arg_1 => ab1 + (ab2 + 1_1)*10_1 , ar => cb)
                       if(arg_1 .ne. ar)then
                       error stop 6
                       endif
                 end associate assoc5
                       if(arg8 .ne. argc8)then
                       error stop 5
                       endif
               end associate assoc4
                       if(arg4 .ne. argc4)then
                       error stop 4
                       endif
             end associate assoc3
                       if(arg2 .ne. argc2)then
                       error stop 3
                       endif
           end associate assoc2
                       if(arg1 .ne. argc1)then
                       error stop 2
                       endif
         end associate assoc1
                       if(arg .ne. argc)then
                       error stop 1
                       endif
      end associate assoc

      assc: associate ( arg => a , arb => (b + 1)*10 )
       arg = arg + (b + 1)*10
        assc1: associate ( arg1 => a1 ,arb1 => (b1 + 1_1)*10_1 )
         arg1 = arg1 + (b1 + 1)*10 
          assc2: associate ( arg2 => a2 , arb2 => (b2 + 1_2)*10_2 )
           arg2 = arg2 + (b2 + 1)*10 
            assc3: associate ( arg4 => a4 , arb4 => (b4 + 1)*10 )
             arg4 = arg4 + (b4 + 1)*10
              assc4: associate ( arg8 => a8 , arb8 => (b8 + 1)*10 )
               arg8 = arg8 + (b8 + 1_8)*10_8
                assc5: associate ( arg_1 => ab1 , arg_2 => (ab2 + 1_1)*10_1 )
                 arg_1 = arg_1 + (ab2 + 1)*10
                       if(arg_1 .ne. cb)then
                       error stop 16
                       endif
                       if(arg_2 .ne. ((ab2 + 1_1)*10_1))then
                       error stop 26
                       endif
                 end associate assc5
                       if(arg8 .ne. c8)then
                       error stop 15
                       endif
                       if(arb8 .ne. ((b8 + 1_8)*10_8))then
                       error stop 25
                       endif
               end associate assc4
                       if(arg4 .ne. c4)then
                       error stop 14
                       endif
                       if(arb4 .ne. ((b4 + 1)*10))then
                       error stop 24
                       endif
             end associate assc3
                       if(arg2 .ne. c2)then
                       error stop 13
                       endif
                       if(arb2 .ne. ((b2 + 1_2)*10_2))then
                       error stop 23
                       endif
           end associate assc2
                       if(arg1 .ne. c1)then
                       error stop 12
                       endif
                       if(arb1 .ne. ((b1 + 1_1)*10_1))then
                       error stop 22
                       endif
         end associate assc1
                       if(arg .ne. c)then
                       error stop 11
                       endif
                       if(arb .ne. ((b + 1)*10))then
                       error stop 21
                       endif
      end associate assc

 end
