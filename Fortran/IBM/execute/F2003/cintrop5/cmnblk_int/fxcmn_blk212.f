!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk212 cxcmn_blk201
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk212 fxcmn_blk212.out
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block wiht BIND(C)
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : February 13, 2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*
!*  REFERENCE                  : Feature 239812
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that scalar variables of
!*                               integer data types inside of common blocks do
!*                               interoperate with C variables
!*
!*                               Scope:  internal subroutine
!* 
!*				 This testcase will also test multiple common blocks with a 
!*			         single variable in them interoperated with C variables, but
!*                               in this testcase all common blocks will be on one COMMON
!*                               statement.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk212
	use iso_c_binding
        implicit none

	call intern_fsub()

   CONTAINS

   subroutine intern_fsub()
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Integers Declaration
!      	- use decimal, binary, octal values to define type
!     	- use KIND, SELECTED_INT_KIND, MAX, LEN
!	- use ISO_C_BINDING modules	
! ----------------------------------------------------------------------------

	integer (kind=o'001')				:: int_s1a 
	integer (LEN('k'))				:: int_s1b

	integer (2 )					:: int_s2a
	integer (kind=2_4) 				:: int_s2b

	integer (kind=b'0100')  			:: int_s4a
        integer (kind=SELECTED_INT_KIND(9))		:: int_s4b

	integer (kind=MAX(8, 7))			:: int_s8a
	integer (kind=int((4.4e0_8,6.5e0_8))+4 ) 	:: int_s8b
	
	INTEGER (C_SIGNED_CHAR 		)		:: int_C_SIGNED_CHAR
	INTEGER (C_SHORT 		)		:: int_C_SHORT
	INTEGER (C_INT 			)		:: int_C_INT
	INTEGER (C_LONG 		)		:: int_C_LONG
	INTEGER (C_LONG_LONG		)		:: int_C_LONG_LONG
	INTEGER (C_SIZE_T 		)		:: int_C_SIZE_T
	INTEGER (C_INTPTR_T 		)		:: int_C_INTPTR_T
	INTEGER (C_INTMAX_T 		)		:: int_C_INTMAX_T
	INTEGER (C_INT8_T 		)		:: int_C_INT8_T
	INTEGER (C_INT16_T 		)		:: int_C_INT16_T 
	INTEGER (C_INT32_T 		)		:: int_C_INT32_T
	INTEGER (C_INT64_T 		)		:: int_C_INT64_T
	INTEGER (C_INT_LEAST8_T 	)		:: int_C_INT_LEAST8_T
	INTEGER (C_INT_LEAST16_T 	)		:: int_C_INT_LEAST16_T
	INTEGER (C_INT_LEAST32_T 	)		:: int_C_INT_LEAST32_T
	INTEGER (C_INT_LEAST64_T 	)		:: int_C_INT_LEAST64_T
	INTEGER (C_INT_FAST8_T 		)		:: int_C_INT_FAST8_T
	INTEGER (C_INT_FAST16_T 	)		:: int_C_INT_FAST16_T
	INTEGER (C_INT_FAST32_T 	)		:: int_C_INT_FAST32_T
	INTEGER (C_INT_FAST64_T 	)		:: int_C_INT_FAST64_T


! ----------------------------------------------------------------------------
! Multiple COMMON statements with one variable in one BIND(C) statements
! ----------------------------------------------------------------------------
        COMMON    /blk_int_s1a/     int_s1a		&
         , /blk_int_s1b/            int_s1b             &
         , /blk_int_s2a/            int_s2a             &
         , /blk_int_s2b/            int_s2b             &
         , /blk_int_s4a/            int_s4a             &
         , /blk_int_s4b/            int_s4b             &
         , /blk_int_s8a/            int_s8a             &
         , /blk_int_s8b/            int_s8b             &
         , /blk_int_C_SIGNED_CHAR/  int_C_SIGNED_CHAR   &
         , /blk_int_C_SHORT/        int_C_SHORT         &
         , /blk_int_C_INT/          int_C_INT           &
         , /blk_int_C_LONG/         int_C_LONG          &
         , /blk_int_C_LONG_LONG/    int_C_LONG_LONG             &
         , /blk_int_C_SIZE_T/       int_C_SIZE_T                &
         , /blk_int_C_INTPTR_T/     int_C_INTPTR_T              &
         , /blk_int_C_INTMAX_T/     int_C_INTMAX_T              &
         , /blk_int_C_INT8_T/       int_C_INT8_T                &
         , /blk_int_C_INT16_T/      int_C_INT16_T               &
         , /blk_int_C_INT32_T/      int_C_INT32_T               &
         , /blk_int_C_INT64_T/      int_C_INT64_T               &
         , /blk_int_C_INT_LEAST8_T/ int_C_INT_LEAST8_T          &
         , /blk_int_C_INT_LEAST16_T/ int_C_INT_LEAST16_T        &
         , /blk_int_C_INT_LEAST32_T/ int_C_INT_LEAST32_T        &
         , /blk_int_C_INT_LEAST64_T/ int_C_INT_LEAST64_T        &
         , /blk_int_C_INT_FAST8_T/   int_C_INT_FAST8_T          &
         , /blk_int_C_INT_FAST16_T/  int_C_INT_FAST16_T         &
         , /blk_int_C_INT_FAST32_T/  int_C_INT_FAST32_T         &
         , /blk_int_C_INT_FAST64_T/  int_C_INT_FAST64_T 


        BIND(C) 	::   /blk_int_s1a/ ,  &
         /blk_int_s1b/ ,  &
         /blk_int_s2a/ ,  &
         /blk_int_s2b/ ,  &
         /blk_int_s4a/ ,  &
         /blk_int_s4b/ ,  &
         /blk_int_s8a/ ,  &
         /blk_int_s8b/ ,  &
         /blk_int_C_SIGNED_CHAR/ ,  &
         /blk_int_C_SHORT/ ,  &
         /blk_int_C_INT/ ,  &
         /blk_int_C_LONG/ ,  &
         /blk_int_C_LONG_LONG/ ,  &
         /blk_int_C_SIZE_T/ ,  &
         /blk_int_C_INTPTR_T/ ,  &
         /blk_int_C_INTMAX_T/ ,  &
         /blk_int_C_INT8_T/  , &
         /blk_int_C_INT16_T/ ,  &
         /blk_int_C_INT32_T/ ,  &
         /blk_int_C_INT64_T/ ,  &
         /blk_int_C_INT_LEAST8_T/ ,  &
         /blk_int_C_INT_LEAST16_T/ ,  &
         /blk_int_C_INT_LEAST32_T/  , &
         /blk_int_C_INT_LEAST64_T/ ,  &
         /blk_int_C_INT_FAST8_T/ ,  &
         /blk_int_C_INT_FAST16_T/ ,  &
         /blk_int_C_INT_FAST32_T/ ,  &
         /blk_int_C_INT_FAST64_T/


! ----------------------------------------------------------------------------
! Integer Initialization
!       - use decimal, binary, octal values to assign values
! ----------------------------------------------------------------------------

        int_s1a 			=  b'1111111'		! d'127'
        int_s1b 			= -128

        int_s2a 			=  o'77777'  		! d'32767'
        int_s2b 			= -32768

        int_s4a 			=  2147483647
        int_s4b 			= -2147483648

        int_s8a 			=  9223372036854775807_8
        int_s8b 			= -9223372036854775807_8

        int_C_SIGNED_CHAR		= 25
        int_C_SHORT			= -31000
        int_C_INT			= -2147483648
        int_C_LONG			= 2147483647 		!-- int*4 with -q32; int*8 with -q64
        int_C_LONG_LONG			= -9223372036854775807_8
        int_C_SIZE_T			= -2000000000           !-- int*4 with -q32; int*8 with -q64
        int_C_INTPTR_T			=  1000000000           !-- int*4 with -q32; int*8 with -q64
        int_C_INTMAX_T			= 9223372036854775807_8
        int_C_INT8_T			= 111
        int_C_INT16_T			= -32768
        int_C_INT32_T			= 2147483647
        int_C_INT64_T			= 9223372036850123456_8
        int_C_INT_LEAST8_T		= -128
        int_C_INT_LEAST16_T		= o'23420'		! d'10000'
        int_C_INT_LEAST32_T		= 0
        int_C_INT_LEAST64_T		= 1111111111111111111_8
        int_C_INT_FAST8_T		= b'001'		! d'1'
        int_C_INT_FAST16_T		= o'100'		! d'64'    
        int_C_INT_FAST32_T 		= 1111111119
        int_C_INT_FAST64_T		= 9223372036854775807_8



! ----------------------------------------------------------------------------
! Integer Verification
!       - verify assigned values before passing to C
! ----------------------------------------------------------------------------

        if ( int_s1a                    .ne.  127                   )    error stop 10
        if ( int_s1b                    .ne. -128                   )    error stop 11
        if ( int_s2a                    .ne.  32767                 )    error stop 12
        if ( int_s2b                    .ne. -32768                 )    error stop 13
        if ( int_s4a                    .ne.  2147483647            )    error stop 14
        if ( int_s4b                    .ne. -2147483648            )    error stop 15
        if ( int_s8a                    .ne.  9223372036854775807_8 )    error stop 16
        if ( int_s8b                    .ne. -9223372036854775807_8 )    error stop 17

        if ( int_c_signed_char          .ne.  25                    )    error stop 18
        if ( int_c_short                .ne. -31000                 )    error stop 19
        if ( int_c_int                  .ne. -2147483648            )    error stop 20
        if ( int_c_long                 .ne.  2147483647            )    error stop 21
        if ( int_c_long_long            .ne. -9223372036854775807_8 )    error stop 22
        if ( int_c_size_t               .ne. -2000000000            )    error stop 23
        if ( int_c_intptr_t             .ne.  1000000000            )    error stop 24
        if ( int_c_intmax_t             .ne.  9223372036854775807_8 )    error stop 25
        if ( int_c_int8_t               .ne.  111                   )    error stop 26
        if ( int_c_int16_t              .ne. -32768                 )    error stop 27
        if ( int_c_int32_t              .ne.  2147483647            )    error stop 28
        if ( int_c_int64_t              .ne.  9223372036850123456_8 )    error stop 29
        if ( int_c_int_least8_t         .ne. -128                   )    error stop 30
        if ( int_c_int_least16_t        .ne.  10000                 )    error stop 31
        if ( int_c_int_least32_t        .ne.  0                     )    error stop 32
        if ( int_c_int_least64_t        .ne.  1111111111111111111_8 )    error stop 33
        if ( int_c_int_fast8_t          .ne.  1                     )    error stop 34
        if ( int_c_int_fast16_t         .ne.  64                    )    error stop 35
        if ( int_c_int_fast32_t         .ne.  1111111119            )    error stop 36
        if ( int_c_int_fast64_t         .ne.  9223372036854775807_8 )    error stop 37


! ----------------------------------------------------------------------------
!  Call to C subprogram  
! ----------------------------------------------------------------------------
	CALL CSUB_ALL()


! ----------------------------------------------------------------------------
! Integer Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------

        if ( int_s1a            .ne. -128                   )    error stop 50
        if ( int_s1b            .ne.  127                   )    error stop 51
        if ( int_s2a            .ne. -32768                 )    error stop 52
        if ( int_s2b            .ne.  32767                 )    error stop 53
        if ( int_s4a            .ne. -2147483647            )    error stop 54
        if ( int_s4b            .ne.  2147483647            )    error stop 55
        if ( int_s8a            .ne. -9223372036854775807_8 )    error stop 56
        if ( int_s8b            .ne. +9223372036854775807_8 )    error stop 57

        if ( int_c_signed_char  .ne.  127                   )    error stop 58
        if ( int_c_short        .ne. +32767                 )    error stop 59
        if ( int_c_int          .ne. +2147483647            )    error stop 60
        if ( int_c_long         .ne. -2147483647            )    error stop 61
        if ( int_c_long_long    .ne.  9223372036854775807_8 )    error stop 62
        if ( int_c_size_t       .ne. -2147483647            )    error stop 63
        if ( int_c_intptr_t     .ne. -2147483647            )    error stop 64
        if ( int_c_intmax_t     .ne.  9223372036854775807_8 )    error stop 65
        if ( int_c_int8_t       .ne. -127                   )    error stop 66
        if ( int_c_int16_t      .ne. +32767                 )    error stop 67
        if ( int_c_int32_t      .ne. -2147483647            )    error stop 68
        if ( int_c_int64_t      .ne. -9223372036850123456_8 )    error stop 69
        if ( int_c_int_least8_t       .ne.  127                   )    error stop 70
        if ( int_c_int_least16_t      .ne. -32767                 )    error stop 71
        if ( int_c_int_least32_t      .ne.  2147483647            )    error stop 72
        if ( int_c_int_least64_t      .ne.  9223372036854775807_8 )    error stop 73
        if ( int_c_int_fast8_t        .ne. -127                   )    error stop 74
        if ( int_c_int_fast16_t       .ne.  6                   )    error stop 75
        if ( int_c_int_fast32_t       .ne. -2147483647            )    error stop 76
        if ( int_c_int_fast64_t       .ne. -9223372036854775807_8 )    error stop 77

   end subroutine

end program
