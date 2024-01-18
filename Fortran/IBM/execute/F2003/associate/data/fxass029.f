!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: fxass029.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass029.f
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
!*                                     and character data types
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

      program fxass27
      implicit none

      logical  precision_x8
      logical precision_r4

      type der

      integer,  allocatable :: i4
      real ,  allocatable :: r4
      complex , allocatable :: x
      logical  :: l4
      character*4 :: c
      byte :: b

      end type der

      type(der) der_type, d_t

      allocate    ( der_type%i4 )
      allocate    ( der_type%r4 )
      allocate    ( der_type%x )

      der_type = der (5,140.8,(1.0,2.0),.false.,'a',8 )
      d_t = der (8,9.6,(2.0,4.0),.true.,'b',10 )
!----------------- ASSOCIATE with DERIVED TYPES  -----------------------

      associate ( dtype => der (5,140.8,(1.0,2.0),.false.,'a',8 ))
            if(dtype%i4 .ne. der_type%i4) error stop 1
            if(dtype%l4 .neqv. der_type%l4) error stop 2
            if (.not. precision_r4(dtype%r4,der_type%r4)) error stop 3
            if (.not. precision_x8(dtype%x,der_type%x)) error stop 4
            if(dtype%b .ne. der_type%b) error stop 5
            if(dtype%c .ne. der_type%c) error stop 6

      end associate

      associate ( dertype => der_type )
            d_t = dertype
      end associate
            if(d_t%i4 .ne. der_type%i4) error stop 7
            if(d_t%l4 .neqv. der_type%l4) error stop 8
            if (.not. precision_r4(d_t%r4,der_type%r4)) error stop 9
            if (.not. precision_x8(d_t%x,der_type%x)) error stop 10
            if(d_t%b .ne. der_type%b) error stop 11
            if(d_t%c .ne. der_type%c) error stop 12
!-----------   ASSOCIATE with INTEGER expressions ----------------

       associate ( dertype => der_type%i4)
            dertype = d_t%i4
       end associate
            if(der_type%i4 .ne. d_t%i4)then
            error stop 13
            endif

!-----------   ASSOCIATE with REAL expressions ----------------
            d_t%r4 = 120.0
       associate ( dertype => der_type%r4 )
            dertype = d_t%r4
       end associate
            if (.not. precision_r4(der_type%r4,d_t%r4)) then
            error stop 14
            endif

      end
