!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass028.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass028.f
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
!*                                     with nested derived types
!*                                     with integer, complex, real
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

      program fxass28
      implicit none

      logical  precision_x8
      logical precision_r4

      type der
       integer*4 i4
       real*4 r4
       complex x
      end type der

      type nest_der
       type(der)  der_type
      end type nest_der

      type(nest_der) der_t
      type(der) d_t

      der_t%der_type%i4 = 5
      der_t%der_type%r4 = 140.8
      der_t%der_type%x = (1.0,2.0)


!-----------   ASSOCIATE with NESTED DERIVED TYPES  -----------------------

      associate ( dtype => der_t )
       if(dtype%der_type%i4 .ne. der_t%der_type%i4) error stop 1
       if (.not.precision_r4(dtype%der_type%r4,der_t%der_type%r4)) error stop 3
       if (.not.precision_x8(dtype%der_type%x,der_t%der_type%x)) error stop 4
      end associate

      associate ( dertype => der_t )
            d_t%i4 = dertype%der_type%i4
            d_t%r4 = dertype%der_type%r4
            d_t%x = der_t%der_type%x
      end associate

            if (d_t%i4 .ne. der_t%der_type%i4) error stop 5
            if (.not. precision_r4(d_t%r4,der_t%der_type%r4)) error stop 6
            if (.not. precision_x8(d_t%x,der_t%der_type%x)) error stop 7

!-----------   ASSOCIATE with INTEGER expressions ----------------
       d_t%i4 = 12
       associate ( dertype => der_t%der_type%i4)
            dertype = d_t%i4
       end associate
            if(der_t%der_type%i4 .ne. d_t%i4)then
            error stop 13
            endif

!-----------   ASSOCIATE with REAL expressions ----------------
       d_t%r4 = 120.0
       associate ( dertype => der_t%der_type%r4 )
            dertype = d_t%r4
       end associate
            if (.not. precision_r4(der_t%der_type%r4,d_t%r4)) then
            error stop 14
            endif

      end
