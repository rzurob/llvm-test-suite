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
!*  DESCRIPTION                : Test: ASSOCIATE with derived types
!*                                     expression with integer
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


      program fxass07
      implicit none

        type der

         character*4 c

         byte b

         logical*1 l1
         logical*2 l2
         logical*4 l4

         integer*1 i1
         integer*2 i2
         integer*4 i4
         integer*8 i8

         real*4 r4
         real*8 r8
         real*16 r16

         complex x
         complex*8 x8
         complex*16 x16

        end type der

        type(der)  der_type, d_t

        logical  precision_x8, precision_x6, precision_x3
        logical precision_r4, precision_r8, precision_r6

!------------------- Initialization of variables --------------------

           der_type%i1 = 5
           d_t%i1 = 8
           der_type%i2 = 15
           d_t%i2 = 30
           der_type%i4 = 14
           d_t%i4 = 11
           der_type%i8 = 17
           d_t%i8 = 20

           der_type%r4 = 4.80
           d_t%r4 = 9.6
           der_type%r8 = 140.8
           d_t%r8 = 281.6
           der_type%r16 = 1600.3
           d_t%r16 = 2400.3

           der_type%l1 = .false.
           d_t%l1 = .true.

           der_type%l2 = .false.
           d_t%l2 = .true.
           der_type%l4 = .true.
           d_t%l4 = .false.

           der_type%c = 'a'
           d_t%c = 'a'

           der_type%b = 8
           d_t%b = 10

           der_type%x = (1.0,2.0)
           der_type%x8 = (3.0E0,4.0E0)
           der_type%x16 = (5.0D0,6.0D0)
           d_t%x = (2.0,4.0)
           d_t%x8 = (6.0E0,8.0E0)
           d_t%x16 = (10.0D0,12.0D0)

!-----------   ASSOCIATE with DERIVED TYPES expressions ----------------
!-----------    ASSOCIATE  with  INTEGER  expressions   ----------------

           associate ( dertype => der_type%i1 + 3_1)
                  if(dertype .ne. d_t%i1)then
                  error stop 10
                  endif
           end associate

           associate ( dertype => der_type%i2*2_2 )
                  if(dertype .ne. d_t%i2)then
                  error stop 11
                  endif
           end associate

           associate ( dertype => der_type%i4 - 3)
                  if(dertype .ne. d_t%i4)then
                  error stop 12
                  endif
           end associate

           associate ( dertype => der_type%i8 + 3_8)
                  if(dertype .ne. d_t%i8)then
                  error stop 13
                  endif
           end associate

!-----------   ASSOCIATE with REAL expressions ----------------

           associate ( dertype => der_type%r4*2 )
                  if (.not. precision_r4(dertype,d_t%r4)) then
                  error stop 14
                  endif
           end associate

           associate ( dertype => der_type%r8*2 )
                  if (.not. precision_r8(dertype,d_t%r8)) then
                  error stop 15
                  endif
           end associate

           associate ( dertype => der_type%r16 + 800.0)
                  if (.not. precision_r6(dertype,d_t%r16)) then
                  error stop 16
                  endif
           end associate

!-----------   ASSOCIATE with CHARACTER expressions ----------------

           associate ( dertype => der_type%c )
                  if(dertype .ne. d_t%c)then
                  error stop 17
                  endif
           end associate

!-----------   ASSOCIATE with BYTE expressions ----------------

           associate ( dertype => der_type%b + 2_1)
                  if(dertype .ne. d_t%b)then
                  error stop 18
                  endif
           end associate

!-----------   ASSOCIATE with COMPLEX expressions ----------------

           associate ( dertype => der_type%x + (1.0,2.0))
                  if (.not. precision_x8(dertype,d_t%x)) then
                  error stop 19
                  endif
           end associate

           associate ( dertype => der_type%x8 + (3.0E0,4.0E0))
                  if (.not. precision_x8(dertype,d_t%x8)) then
                  error stop 20
                  endif
           end associate

      end
