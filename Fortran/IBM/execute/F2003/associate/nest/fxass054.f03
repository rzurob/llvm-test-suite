!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,logical
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
!*  DESCRIPTION                : Test: NESTED ASSOCIATE with expressions
!*                                     with logical, logical*(1,2,4,8) and
!*                                     character data types.
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

@PROCESS CTYPLSS

      program fxass54a
      implicit none

      logical a / .true.  /
      logical b / .false. /
      logical c / .true. /

      logical*1 a1 / .true. /
      logical*1 b1 / .false. /
      logical*1 c1 / .false. /

      logical*2 a2 / .true. /
      logical*2 b2 / .false. /
      logical*2 c2 / .true. /

      logical*4 a4 / .true. /
      logical*4 b4 / .false. /
      logical*4 c4 / .false. /

      logical*8 a8 / .true. /
      logical*8 b8 / .false. /
      logical*8 c8 / .true. /

      character*4 ca4 / 'good' /

      character*9 ca8 / 'excellent' /

      character*5 ca5 / 'white' /

!-----------   ASSOCIATE with LOGICAL expressions ----------------

      assoc: associate ( arg => a )
        assoc1: associate ( arg1 => a1 )
           assoc2: associate ( arg2 => a2 )
             assoc3: associate ( arg4 => a4 )
               assoc4 : associate ( arg8 => a8 )
                 if(arg8 .neqv. a8)then
                 error stop 5
                 endif
               end associate assoc4
                 if(arg4 .neqv. a4)then
                 error stop 4
                 endif
             end associate assoc3
                 if(arg2 .neqv. a2)then
                 error stop 3
                 endif
           end associate assoc2
                 if(arg1 .neqv. a1)then
                 error stop 2
                 endif
         end associate assoc1
                 if(arg .neqv. a)then
                 error stop 1
                 endif
      end associate assoc

      assc: associate ( ar => a .neqv. b )
        assc1: associate ( ar1 => a1 .eqv. b1 )
           assc2: associate ( ar2 => a2 .neqv. b2 )
             assc3: associate ( ar4 => a4 .eqv. b4 )
               assc4: associate ( ar8 => a8 .neqv. b8 )
                 if(ar8 .neqv. c8)then
                 error stop 15
                 endif
               end associate assc4
                 if(ar4 .neqv. c4)then
                 error stop 14
                 endif
             end associate assc3
                 if(ar2 .neqv. c2)then
                 error stop 13
                 endif
           end associate assc2
                 if(ar1 .neqv. c1)then
                 error stop 12
                 endif
         end associate assc1
                 if(ar .neqv. c)then
                 error stop 11
                 endif
      end associate assc

      assoc_1: associate ( arg_4 => ca4 )
        assoc_2: associate ( arg_8 => ca8 )
          assoc_3: associate ( arg5 =>  ca5 )
                 if(arg5 .ne. ca5)then
                 error stop 16
                 endif
          end associate assoc_3
                 if(arg_8 .ne. ca8)then
                 error stop 17
                 endif
        end associate assoc_2
                 if(arg_4 .ne. ca4)then
                 error stop 18
                 endif
      end associate assoc_1

     end
