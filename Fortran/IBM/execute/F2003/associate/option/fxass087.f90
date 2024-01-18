!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90 -qrealsize=4
! %GROUP: fxass087.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass087.f
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
!*                                     with derived types with integer
!*                                     logical, complex, real, byte
!*                                     and character data types using
!*                                     different -qrealsize options.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass87
      implicit none

       type der

         real r4
         real r8
         real r16

        end type der

        type(der)  der_type, d_t

        logical :: precision_r4

!------------------- Initialization of variables --------------------

           der_type%r4 = 4.80
           d_t%r4 = 9.6
           der_type%r8 = 140.8
           d_t%r8 = 281.6
           der_type%r16 = 1600.3
           d_t%r16 = 2400.3

!-----------   ASSOCIATE with REAL expressions ----------------

           associate ( dertype => der_type%r4 )
                  dertype = dertype*2.0
                  if (.not.precision_r4(dertype,der_type%r4)) then
                  error stop 14
                  endif
           end associate

           associate ( dertype => der_type%r8 * 2.0)
                  if (.not.precision_r4(dertype,d_t%r8)) then
                  error stop 15
                  endif
           end associate

           associate ( dertype => 1600.3 )
                  if (.not.precision_r4(dertype,1600.3)) then
                  error stop 16
                  endif
           end associate

      end

