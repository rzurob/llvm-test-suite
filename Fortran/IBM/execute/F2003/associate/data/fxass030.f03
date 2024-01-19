!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE, derived types
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
!*  DESCRIPTION                : Test: ASSOCIATE with expressions
!*                                     with kind(1,2,4,8,16) integer
!*                                     logical, complex, real data types
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

      program fxass30
      implicit none

         logical(kind=1) l1
         logical(kind=2) l2
         logical(kind=4) l4

         integer(kind=1) i1
         integer(kind=2) i2
         integer(kind=4) i4
         integer(kind=8) i8

         real(kind=4) r4
         real(kind=8) r8
         real(kind=16) r16

         complex(kind=4) x
         complex(kind=8) x8
         complex(kind=16) x16

        logical  precision_x8, precision_x6, precision_x3
        logical precision_r4, precision_r8, precision_r6

!------------------- Initialization of variables --------------------

           i1 = 5
           i2 = 15
           i4 = 14
           i8 = 17

           r4 = 4.80
           r8 = 140.8
           r16 = 1600.3

           l1 = .false.
           l2 = .false.
           l4 = .true.

           x = (1.0,2.0)
           x8 = (3.0E0,4.0E0)
           x16 = (5.0D0,6.0D0)

!-----------    ASSOCIATE  with  INTEGER  expressions   ----------------

           associate ( arg => i1 )
                  if(arg .ne. i1)then
                  error stop 10
                  endif
           end associate

           associate ( arg => i2 )
                  arg = arg*2
                  if(arg .ne. i2)then
                  error stop 11
                  endif
           end associate

           associate ( arg => (i4 - 3) )
                  if(arg .ne. (i4 - 3))then
                  error stop 12
                  endif
           end associate

           associate ( arg => i8 )
                  arg = arg + 3
                  if(arg .ne. i8)then
                  error stop 13
                  endif
           end associate

!-----------   ASSOCIATE with REAL expressions ----------------

           associate ( arg => r4 )
                  arg = arg*2
                  if (.not. precision_r4(arg,r4)) then
                  error stop 14
                  endif
           end associate

           associate ( arg => r8 )
                  if (.not. precision_r8(arg,r8)) then
                  error stop 15
                  endif
           end associate

           associate ( arg => r16 + 800.0)
                  if (.not. precision_r6(arg,(r16+800.0))) then
                  error stop 16
                  endif
           end associate

!-----------   ASSOCIATE with COMPLEX expressions ----------------

           associate ( arg => x )
                  if (.not. precision_x8(arg,x)) then
                  error stop 19
                  endif
           end associate

           associate ( arg => x8 )
                  arg = arg + (3.0E0,4.0E0)
                  if (.not. precision_x8(arg,x8)) then
                  error stop 20
                  endif
           end associate

      end
