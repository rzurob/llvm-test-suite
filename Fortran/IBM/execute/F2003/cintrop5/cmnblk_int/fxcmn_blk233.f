!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk233 cxcmn_blk203
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: fxcmn_blk233.out
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk233 fxcmn_blk233.out
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block with BIND(C)
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
!*  DESCRIPTION                : This test case will verify that variables of
!*                               integer data types inside of common blocks do
!*                               interoperate with C variables
!*
!*                               Scope:  module
!*
!*				This testcase will verify array variables.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module fmod1 
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Integers Declaration
!      	- use decimal, binary, octal values to define type
!     	- use KIND, SELECTED_INT_KIND, MAX, LEN
!	- use ISO_C_BINDING modules	
! ----------------------------------------------------------------------------

	integer (kind=o'001')				:: int_s1a(5) 
	integer (LEN('k'))				:: int_s1b(5)

	integer (2 )					:: int_s2a(5)
	integer (kind=2_4) 				:: int_s2b(5)

	integer (kind=b'0100')  			:: int_s4a(5)
        integer (kind=SELECTED_INT_KIND(9))		:: int_s4b(5)

	integer (kind=MAX(8, 7))			:: int_s8a(5)
	integer (kind=int((4.4e0_8,6.5e0_8))+4 ) 	:: int_s8b(5)
	
	INTEGER (C_SIGNED_CHAR 		)		:: int_C_SIGNED_CHAR(5)
	INTEGER (C_SHORT 		)		:: int_C_SHORT(5)
	INTEGER (C_INT 			)		:: int_C_INT(5)
	INTEGER (C_LONG 		)		:: int_C_LONG(5)
	INTEGER (C_LONG_LONG		)		:: int_C_LONG_LONG(5)
	INTEGER (C_SIZE_T 		)		:: int_C_SIZE_T(5)
	INTEGER (C_INTPTR_T 		)		:: int_C_INTPTR_T(5)
	INTEGER (C_INTMAX_T 		)		:: int_C_INTMAX_T(5)
	INTEGER (C_INT8_T 		)		:: int_C_INT8_T(5)
	INTEGER (C_INT16_T 		)		:: int_C_INT16_T(5)
	INTEGER (C_INT32_T 		)		:: int_C_INT32_T(5)
	INTEGER (C_INT64_T 		)		:: int_C_INT64_T(5)
	INTEGER (C_INT_LEAST8_T 	)		:: int_C_INT_LEAST8_T(5)
	INTEGER (C_INT_LEAST16_T 	)		:: int_C_INT_LEAST16_T(5)
	INTEGER (C_INT_LEAST32_T 	)		:: int_C_INT_LEAST32_T(5)
	INTEGER (C_INT_LEAST64_T 	)		:: int_C_INT_LEAST64_T(5)
	INTEGER (C_INT_FAST8_T 		)		:: int_C_INT_FAST8_T(5)
	INTEGER (C_INT_FAST16_T 	)		:: int_C_INT_FAST16_T(5)
	INTEGER (C_INT_FAST32_T 	)		:: int_C_INT_FAST32_T(5)
	INTEGER (C_INT_FAST64_T 	)		:: int_C_INT_FAST64_T(5)


! ----------------------------------------------------------------------------
! COMMON and BIND(C) statements
! ----------------------------------------------------------------------------
	common /blk_all/ int_s1a, int_s1b, int_s2a, int_s2b, int_s4a, int_s4b, int_s8a, int_s8b, int_C_SIGNED_CHAR, int_C_SHORT, 	&
			 int_C_INT, int_C_LONG, int_C_LONG_LONG, int_C_SIZE_T, int_C_INTPTR_T, int_C_INTMAX_T, int_C_INT8_T, 		&
			 int_C_INT16_T, int_C_INT32_T, int_C_INT64_T, int_C_INT_LEAST8_T, int_C_INT_LEAST16_T, int_C_INT_LEAST32_T, 	&
			 int_C_INT_LEAST64_T, int_C_INT_FAST8_T, int_C_INT_FAST16_T, int_C_INT_FAST32_T, int_C_INT_FAST64_T

        bind(c) ::   /blk_all/

end module fmod1 


program fxcmn_blk233
	use fmod1
	implicit none


! ----------------------------------------------------------------------------
! Integer Initialization
!       - use decimal, binary, octal values to assign values
!       - for some variables, assign 1 value to whole array; for others, assign
!         specific values for each element in the array
! ----------------------------------------------------------------------------

        int_s1a 			=  (/b'1111111',o'0',-128,o'177',127/)		
        int_s1b 			= -128

        int_s2a 			=  (/o'77777',o'0',-32768, 32767,b'1111111'/)
        int_s2b 			= -32768

        int_s4a 			=  (/2147483647,b'1111111',-2147483648, 0, o'3641100'/) 
        int_s4b 			= -2147483648

        int_s8a 			=  (/9223372036854775807_8,b'000000000',-9223372036854775807_8, o'3641100', -2147483648_8/)
        int_s8b 			= -9223372036854775807_8

        int_C_SIGNED_CHAR		= (/b'1111111',o'0',-128, o'177',127/)
        int_C_SHORT			= (/o'77777',o'0',-32768, 32767,b'1111111'/)
        int_C_INT			= (/2147483647,b'1111111',-2147483648, 0, o'3641100'/)
        int_C_LONG			= 2147483647 
        int_C_LONG_LONG			= -9223372036854775807_8
        int_C_SIZE_T			= (/2147483647,b'1111111',-2147483648, 0, o'3641100'/)
        int_C_INTPTR_T			=  1000000000           
        int_C_INTMAX_T			= (/9223372036854775807_8,b'000000000',-9223372036854775807_8, o'3641100', -2147483648_8/)
        int_C_INT8_T			= -128
        int_C_INT16_T			= (/o'77777',o'0',-32768, 32767,b'1111111'/)
        int_C_INT32_T			= 2147483647
        int_C_INT64_T			= 9223372036850123456_8
        int_C_INT_LEAST8_T		= -128
        int_C_INT_LEAST16_T		= (/o'77777',o'0',-32768, 32767,b'1111111'/)
        int_C_INT_LEAST32_T		= 0
        int_C_INT_LEAST64_T		= 1111111111111111111_8
        int_C_INT_FAST8_T		= b'001'		! d'1'
        int_C_INT_FAST16_T		= o'100'		! d'64'   
        int_C_INT_FAST32_T 		= (/2147483647,b'1111111',-2147483648, 0, o'3641100'/)
        int_C_INT_FAST64_T		= (/9223372036854775807_8,b'000000000',-9223372036854775807_8, o'3641100', -2147483648_8/)



! ----------------------------------------------------------------------------
! Integer Verification
!       - verify assigned values before passing to C
! ----------------------------------------------------------------------------

        if ( int_s1a(1)         .ne.  b'1111111'                )    error stop 5
        if ( int_s1a(2)         .ne.  o'0'                      )    error stop 5
        if ( int_s1a(3)         .ne.  -128                      )    error stop 5
        if ( int_s1a(4)         .ne.  o'177'                    )    error stop 5
        if ( int_s1a(5)         .ne.  127                       )    error stop 5

        if ( ANY (int_s1b(1:5)  .ne. -128                       ))   error stop 6

        if ( int_s2a(1)         .ne.  o'77777'                  )    error stop 7
        if ( int_s2a(2)         .ne.  o'0'                      )    error stop 7
        if ( int_s2a(3)         .ne.  -32768                    )    error stop 7
        if ( int_s2a(4)         .ne.  32767                     )    error stop 7
        if ( int_s2a(5)         .ne.  b'1111111'                )    error stop 7

        if ( ANY (int_s2b(1:5)  .ne. -32768                     ))   error stop 8

        if ( int_s4a(1)         .ne.  2147483647                )    error stop 9
        if ( int_s4a(2)         .ne.  b'1111111'                )    error stop 9
        if ( int_s4a(3)         .ne. -2147483648                )    error stop 9
        if ( int_s4a(4)         .ne.  0                         )    error stop 9
        if ( int_s4a(5)         .ne.  o'3641100'                )    error stop 9

        if ( ANY (int_s4b(1:5)  .ne. -2147483648                ))   error stop 10

        if ( int_s8a(1)         .ne.  9223372036854775807_8     )    error stop 11
        if ( int_s8a(2)         .ne.  b'000000000'              )    error stop 11
        if ( int_s8a(3)         .ne. -9223372036854775807_8     )    error stop 11
        if ( int_s8a(4)         .ne.  o'3641100'                )    error stop 11
        if ( int_s8a(5)         .ne. -2147483648_8              )    error stop 11

        if ( ANY (int_s8b(1:5)  .ne. -9223372036854775807_8     ))   error stop 12

        if ( int_c_signed_char(1)       .ne.  b'1111111'            )    error stop 13
        if ( int_c_signed_char(2)       .ne.  o'0'                  )    error stop 13
        if ( int_c_signed_char(3)       .ne. -128                   )    error stop 13
        if ( int_c_signed_char(4)       .ne.  o'177'                )    error stop 13
        if ( int_c_signed_char(5)       .ne.  127                   )    error stop 13

        if ( int_c_short(1)             .ne.  o'77777'              )    error stop 14
        if ( int_c_short(2)             .ne.  o'0'                  )    error stop 14
        if ( int_c_short(3)             .ne. -32768                 )    error stop 14
        if ( int_c_short(4)             .ne.  32767                 )    error stop 14
        if ( int_c_short(5)             .ne.  b'1111111'            )    error stop 14

        if ( int_c_int(1)               .ne.  2147483647            )    error stop 15
        if ( int_c_int(2)               .ne.  b'1111111'            )    error stop 15
        if ( int_c_int(3)               .ne. -2147483648            )    error stop 15
        if ( int_c_int(4)               .ne.  0                     )    error stop 15
        if ( int_c_int(5)               .ne.  o'3641100'            )    error stop 15

        if ( ANY (int_c_long(1:5)       .ne.  2147483647            ))   error stop 16

        if ( ANY (int_c_long_long(1:5)  .ne. -9223372036854775807_8 ))   error stop 17

        if ( int_c_size_t(1)            .ne.  2147483647            )    error stop 18
        if ( int_c_size_t(2)            .ne.  b'1111111'            )    error stop 18
        if ( int_c_size_t(3)            .ne. -2147483648            )    error stop 18
        if ( int_c_size_t(4)            .ne.  0                     )    error stop 18
        if ( int_c_size_t(5)            .ne.  o'3641100'            )    error stop 18
        if ( ANY (int_c_intptr_t(1:5)   .ne.  1000000000            ))   error stop 19

        if ( int_c_intmax_t(1)          .ne.  9223372036854775807_8 )    error stop 20
        if ( int_c_intmax_t(2)          .ne.  b'000000000'          )    error stop 20
        if ( int_c_intmax_t(3)          .ne. -9223372036854775807_8 )    error stop 20
        if ( int_c_intmax_t(4)          .ne.  o'3641100'            )    error stop 20
        if ( int_c_intmax_t(5)          .ne. -2147483648_8          )    error stop 20

        if ( ANY (int_c_int8_t(1:5)     .ne. -128                   ))   error stop 21

        if ( int_c_int16_t(1)           .ne.  o'77777'              )    error stop 22
        if ( int_c_int16_t(2)           .ne.  o'0'                  )    error stop 22
        if ( int_c_int16_t(3)           .ne. -32768                 )    error stop 22
        if ( int_c_int16_t(4)           .ne.  32767                 )    error stop 22
        if ( int_c_int16_t(5)           .ne.  b'1111111'            )    error stop 22

        if ( ANY (int_c_int32_t(1:5)    .ne.  2147483647            ))   error stop 23

        if ( ANY (int_c_int64_t(1:5)    .ne.  9223372036850123456_8 ))   error stop 24

        if ( ANY (int_c_int_least8_t(1:5) .ne. -128                 ))   error stop 25

        if ( int_c_int_least16_t(1)   .ne.  o'77777'                )    error stop 26
        if ( int_c_int_least16_t(2)   .ne.  o'0'                    )    error stop 26
        if ( int_c_int_least16_t(3)   .ne. -32768                   )    error stop 26
        if ( int_c_int_least16_t(4)   .ne.  32767                   )    error stop 26
        if ( int_c_int_least16_t(5)   .ne.  b'1111111'              )    error stop 26

        if ( ANY (int_c_int_least32_t(1:5) .ne.  0                 ))    error stop 27

        if ( ANY (int_c_int_least64_t(1:5) .ne. 1111111111111111111_8 ))  error stop 28

        if ( ANY (int_c_int_fast8_t(1:5)   .ne. b'001'              ))    error stop 29

        if ( ANY (int_c_int_fast16_t(1:5)   .ne. 64                 ))    error stop 168

        if ( int_c_int_fast32_t(1)    .ne.  2147483647              )    error stop 30
        if ( int_c_int_fast32_t(2)    .ne.  b'1111111'              )    error stop 30
        if ( int_c_int_fast32_t(3)    .ne. -2147483648              )    error stop 30
        if ( int_c_int_fast32_t(4)    .ne.  0                       )    error stop 30
        if ( int_c_int_fast32_t(5)    .ne.  o'3641100'              )    error stop 30

        if ( int_c_int_fast64_t(1)    .ne.  9223372036854775807_8   )    error stop 31
        if ( int_c_int_fast64_t(2)    .ne.  b'000000000'            )    error stop 31
        if ( int_c_int_fast64_t(3)    .ne. -9223372036854775807_8   )    error stop 31
        if ( int_c_int_fast64_t(4)    .ne.  o'3641100'              )    error stop 31
        if ( int_c_int_fast64_t(5)    .ne. -2147483648_8            )    error stop 31



! ----------------------------------------------------------------------------
!  Call to C subprogram
! ----------------------------------------------------------------------------
        CALL CSUB_ALL()


! ----------------------------------------------------------------------------
! Integer Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------

        if ( int_s1a(5)         .ne.  b'1111111'                )    error stop 32
        if ( int_s1a(4)         .ne.  o'0'                      )    error stop 32
        if ( int_s1a(3)         .ne.  -128                      )    error stop 32
        if ( int_s1a(2)         .ne.  o'177'                    )    error stop 32
        if ( int_s1a(1)         .ne.  127                       )    error stop 32

        if ( ANY (int_s1b(1:5)  .ne. 127                        ))   error stop 33

        if ( int_s2a(5)         .ne.  o'77777'                  )    error stop 34
        if ( int_s2a(4)         .ne.  o'0'                      )    error stop 34
        if ( int_s2a(3)         .ne.  -32768                    )    error stop 34
        if ( int_s2a(2)         .ne.  32767                     )    error stop 34
        if ( int_s2a(1)         .ne.  b'1111111'                )    error stop 34

        if ( ANY (int_s2b(1:5)  .ne. 32767                     ))   error stop 35

        if ( int_s4a(5)         .ne.  2147483647                )    error stop 36
        if ( int_s4a(4)         .ne.  b'1111111'                )    error stop 36
        if ( int_s4a(3)         .ne. -2147483648                )    error stop 36
        if ( int_s4a(2)         .ne.  0                         )    error stop 36
        if ( int_s4a(1)         .ne.  o'3641100'                )    error stop 36

        if ( ANY (int_s4b(1:5)  .ne. 2147483647                ))   error stop 37

        if ( int_s8a(5)         .ne.  9223372036854775807_8     )    error stop 38
        if ( int_s8a(4)         .ne.  b'000000000'              )    error stop 38
        if ( int_s8a(3)         .ne. -9223372036854775807_8     )    error stop 38
        if ( int_s8a(2)         .ne.  o'3641100'                )    error stop 38
        if ( int_s8a(1)         .ne. -2147483648_8              )    error stop 38

        if ( ANY (int_s8b(1:5)  .ne. 9223372036854775807_8     ))   error stop 39

        if ( int_c_signed_char(5)       .ne.  b'1111111'            )    error stop 40
        if ( int_c_signed_char(4)       .ne.  o'0'                  )    error stop 40
        if ( int_c_signed_char(3)       .ne. -128                   )    error stop 40
        if ( int_c_signed_char(2)       .ne.  o'177'                )    error stop 40
        if ( int_c_signed_char(1)       .ne.  127                   )    error stop 40

        if ( int_c_short(5)             .ne.  o'77777'              )    error stop 41
        if ( int_c_short(4)             .ne.  o'0'                  )    error stop 41
        if ( int_c_short(3)             .ne. -32768                 )    error stop 41
        if ( int_c_short(2)             .ne.  32767                 )    error stop 41
        if ( int_c_short(1)             .ne.  b'1111111'            )    error stop 41

        if ( int_c_int(5)               .ne.  2147483647            )    error stop 42
        if ( int_c_int(4)               .ne.  b'1111111'            )    error stop 42
        if ( int_c_int(3)               .ne. -2147483648            )    error stop 42
        if ( int_c_int(2)               .ne.  0                     )    error stop 42
        if ( int_c_int(1)               .ne.  o'3641100'            )    error stop 42

        if ( ANY (int_c_long(1:5)       .ne. -2147483648            ))   error stop 43

        if ( ANY (int_c_long_long(1:5)  .ne. -9223372036854775807_8 ))   error stop 44


        if ( int_c_size_t(5)            .ne.  2147483647            )    error stop 45
        if ( int_c_size_t(4)            .ne.  b'1111111'            )    error stop 45
        if ( int_c_size_t(3)            .ne.  2147483646            )    error stop 45
        if ( int_c_size_t(2)            .ne.  0                     )    error stop 45
        if ( int_c_size_t(1)            .ne.  o'3641100'            )    error stop 45

        if ( ANY (int_c_intptr_t(1:5)   .ne.  -1000000000           ))   error stop 46

        if ( int_c_intmax_t(5)          .ne.  9223372036854775807_8 )    error stop 47
        if ( int_c_intmax_t(4)          .ne.  b'000000000'          )    error stop 47
        if ( int_c_intmax_t(3)          .ne. -9223372036854775807_8 )    error stop 47
        if ( int_c_intmax_t(2)          .ne.  o'3641100'            )    error stop 47
        if ( int_c_intmax_t(1)          .ne. -2147483648_8          )    error stop 47

        if ( ANY (int_c_int8_t(1:5)     .ne. 127                   ))   error stop 48

        if ( int_c_int16_t(5)           .ne.  o'77777'              )    error stop 49
        if ( int_c_int16_t(4)           .ne.  o'0'                  )    error stop 49
        if ( int_c_int16_t(3)           .ne. -32768                 )    error stop 49
        if ( int_c_int16_t(2)           .ne.  32767                 )    error stop 49
        if ( int_c_int16_t(1)           .ne.  b'1111111'            )    error stop 49

        if ( ANY (int_c_int32_t(1:5)    .ne. -2147483648            ))   error stop 50

        if ( ANY (int_c_int64_t(1:5)    .ne. -9223372036850123456_8 ))   error stop 51

        if ( ANY (int_c_int_least8_t(1:5) .ne. 127                 ))   error stop 52

        if ( int_c_int_least16_t(5)   .ne.  o'77777'                )    error stop 53
        if ( int_c_int_least16_t(4)   .ne.  o'0'                    )    error stop 53
        if ( int_c_int_least16_t(3)   .ne. -32768                   )    error stop 53
        if ( int_c_int_least16_t(2)   .ne.  32767                   )    error stop 53
        if ( int_c_int_least16_t(1)   .ne.  b'1111111'              )    error stop 53

        if ( ANY (int_c_int_least32_t(1:5) .ne.  2147483647         ))   error stop 54

        if ( ANY (int_c_int_least64_t(1:5) .ne. -111111111111111111_8 )) error stop 55

        if ( ANY (int_c_int_fast8_t(1:5)   .ne. -127                ))   error stop 56

        if ( ANY (int_c_int_fast16_t(1:5)   .ne. 6                 ))    error stop 169

        if ( int_c_int_fast32_t(5)    .ne.  2147483647              )    error stop 57
        if ( int_c_int_fast32_t(4)    .ne.  b'1111111'              )    error stop 57
        if ( int_c_int_fast32_t(3)    .ne. -2147483648              )    error stop 57
        if ( int_c_int_fast32_t(2)    .ne.  0                       )    error stop 57
        if ( int_c_int_fast32_t(1)    .ne.  o'3641100'              )    error stop 57

        if ( int_c_int_fast64_t(5)    .ne.  9223372036854775807_8   )    error stop 58
        if ( int_c_int_fast64_t(4)    .ne.  b'000000000'            )    error stop 58
        if ( int_c_int_fast64_t(3)    .ne. -9223372036854775807_8   )    error stop 58
        if ( int_c_int_fast64_t(2)    .ne.  o'3641100'              )    error stop 58
        if ( int_c_int_fast64_t(1)    .ne. -2147483648_8            )    error stop 58

end program

