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
! %GROUP: fxass048.f
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
!*  TEST CASE NAME             : fxass048.f
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
!*  DESCRIPTION                : Test: NESTED ASSOCIATE
!*                                     with expressions and with
!*                                     integer, real data types.
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

      program fxass048
      implicit none

      integer a / 1 /
      integer b / 4 /
      integer c

      real aa / 1.9 /
      real bb / 4.4 /
      real cc
      logical :: precision_r4

      c = a + (b + 1)*10 
      cc = aa + (bb + 1)*10.0 

!-----------   ASSOCIATE with INTEGER expressions ----------------

      assoc: associate ( arg => a + (b + 1)*10 )
        assoc1: associate ( arg => c )
                       if(arg .ne. c)then
                       error stop 2
                       endif
         end associate assoc1
                       if(arg .ne. (a + (b + 1)*10))then
                       error stop 1
                       endif
      end associate assoc

      assc: associate ( ar => a )
        assc1: associate ( ar => b )
                       if(ar .ne. b)then
                       error stop 3
                       endif
         end associate assc1
                       if(ar .ne. a)then
                       error stop 4
                       endif
      end associate assc

      ac: associate ( ag1 => a )
        ac1: associate ( ag => ag1 )
                       if(ag .ne. a)then
                       error stop 5
                       endif
         end associate ac1
                       if(ag1 .ne. a)then
                       error stop 6
                       endif
      end associate ac

!-----------   ASSOCIATE with REAL expressions ----------------

      look1: associate ( rg => aa + (bb + 1)*10.0 )
        look2: associate ( rg => cc )
                       if (.not. precision_r4(rg,cc)) then
                       error stop 7
                       endif
         end associate look2
                       if (.not. precision_r4(rg,cc)) then
                       error stop 8
                       endif
      end associate look1

      look3: associate ( aar => aa )
        look4: associate ( aar => bb )
                       if (.not. precision_r4(aar,bb)) then
                       error stop 9
                       endif
         end associate look4
                       if (.not. precision_r4(aar,aa)) then
                       error stop 10
                       endif
      end associate look3

      look5: associate ( agg1 => aa )
        look6: associate ( agg => agg1 )
                       if (.not. precision_r4(agg,aa)) then
                       error stop 11
                       endif
         end associate look6
                       if (.not. precision_r4(agg1,aa)) then
                       error stop 12
                       endif
      end associate look5

      end
