!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 13, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  REFERENCE                  : Feature 239812
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that variables of
!*                               integer data types inside of common blocks do
!*                               interoperate with C variables
!*
!*                               Scope:  module
!*
!*                               This testcase will test multiple common blocks with a
!*                               single variable in them interoperated with C variables.
!*				 (i.e. no struct used in C code)
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
!	INTEGER (C_INT_FAST16_T 	)		:: int_C_INT_FAST16_T(5)
	INTEGER (C_INT_FAST32_T 	)		:: int_C_INT_FAST32_T(5)
	INTEGER (C_INT_FAST64_T 	)		:: int_C_INT_FAST64_T(5)


! ----------------------------------------------------------------------------
! Multiple COMMON statements with one variable in one BIND(C) statements
! ----------------------------------------------------------------------------

         common /blk_int_s1a/            int_s1a
         common /blk_int_s1b/            int_s1b

         common /blk_int_s2a/            int_s2a
         common /blk_int_s2b/            int_s2b

         common /blk_int_s4a/            int_s4a
         common /blk_int_s4b/            int_s4b

         common /blk_int_s8a/            int_s8a
         common /blk_int_s8b/            int_s8b

         common /blk_int_C_SIGNED_CHAR/  int_C_SIGNED_CHAR
         common /blk_int_C_SHORT/        int_C_SHORT
         common /blk_int_C_INT/          int_C_INT
         common /blk_int_C_LONG/         int_C_LONG
         common /blk_int_C_LONG_LONG/    int_C_LONG_LONG
         common /blk_int_C_SIZE_T/       int_C_SIZE_T
         common /blk_int_C_INTPTR_T/     int_C_INTPTR_T
         common /blk_int_C_INTMAX_T/     int_C_INTMAX_T
         common /blk_int_C_INT8_T/       int_C_INT8_T
         common /blk_int_C_INT16_T/      int_C_INT16_T
         common /blk_int_C_INT32_T/      int_C_INT32_T
         common /blk_int_C_INT64_T/      int_C_INT64_T
         common /blk_int_C_INT_LEAST8_T/ int_C_INT_LEAST8_T
         common /blk_int_C_INT_LEAST16_T/ int_C_INT_LEAST16_T
         common /blk_int_C_INT_LEAST32_T/ int_C_INT_LEAST32_T
         common /blk_int_C_INT_LEAST64_T/ int_C_INT_LEAST64_T
         common /blk_int_C_INT_FAST8_T/   int_C_INT_FAST8_T
!         common /blk_int_C_INT_FAST16_T/  int_C_INT_FAST16_T
         common /blk_int_C_INT_FAST32_T/  int_C_INT_FAST32_T
         common /blk_int_C_INT_FAST64_T/  int_C_INT_FAST64_T


        bind(c) ::   /blk_int_s1a/ ,  &
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
         /blk_int_C_INT_FAST32_T/ ,  &
         /blk_int_C_INT_FAST64_T/

end module fmod1


program fxcmn_blk234
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
!        int_C_INT_FAST16_T		= o'100'		! d'64'     ! Only for AIX52, Mac, and Linux
        int_C_INT_FAST32_T 		= (/2147483647,b'1111111',-2147483648, 0, o'3641100'/)
        int_C_INT_FAST64_T		= (/9223372036854775807_8,b'000000000',-9223372036854775807_8, o'3641100', -2147483648_8/)



! ----------------------------------------------------------------------------
! Integer Verification
!       - verify assigned values before passing to C
! ----------------------------------------------------------------------------

        if ( int_s1a(1)        	.ne.  b'1111111'      		)    error stop 10
        if ( int_s1a(2)        	.ne.  o'0'       		)    error stop 11
        if ( int_s1a(3)        	.ne.  -128       		)    error stop 12
        if ( int_s1a(4)        	.ne.  o'177'      		)    error stop 13
        if ( int_s1a(5)        	.ne.  127       		)    error stop 14

        if ( ANY (int_s1b(1:5)  .ne. -128                   	))   error stop 15

        if ( int_s2a(1)        	.ne.  o'77777'      		)    error stop 16
        if ( int_s2a(2)        	.ne.  o'0'       		)    error stop 17
        if ( int_s2a(3)        	.ne.  -32768      		)    error stop 18
        if ( int_s2a(4)        	.ne.  32767      		)    error stop 19
        if ( int_s2a(5)        	.ne.  b'1111111'      		)    error stop 20

        if ( ANY (int_s2b(1:5)  .ne. -32768               	))   error stop 21

        if ( int_s4a(1)         .ne.  2147483647            	)    error stop 22
        if ( int_s4a(2)         .ne.  b'1111111'            	)    error stop 23
        if ( int_s4a(3)         .ne. -2147483648            	)    error stop 24
        if ( int_s4a(4)         .ne.  0             		)    error stop 25
        if ( int_s4a(5)         .ne.  o'3641100'            	)    error stop 26

        if ( ANY (int_s4b(1:5)  .ne. -2147483648            	))   error stop 27

        if ( int_s8a(1)         .ne.  9223372036854775807_8 	)    error stop 28
        if ( int_s8a(2)         .ne.  b'000000000' 		)    error stop 29
        if ( int_s8a(3)         .ne. -9223372036854775807_8 	)    error stop 30
        if ( int_s8a(4)         .ne.  o'3641100' 		)    error stop 31
        if ( int_s8a(5)         .ne. -2147483648_8   		)    error stop 32

        if ( ANY (int_s8b(1:5)  .ne. -9223372036854775807_8 	))   error stop 33

        if ( int_c_signed_char(1)    	.ne.  b'1111111'            )    error stop 34
        if ( int_c_signed_char(2)    	.ne.  o'0' 	            )    error stop 35
        if ( int_c_signed_char(3)    	.ne. -128 	            )    error stop 36
        if ( int_c_signed_char(4)    	.ne.  o'177'                )    error stop 37
        if ( int_c_signed_char(5)    	.ne.  127  	            )    error stop 38

        if ( int_c_short(1)             .ne.  o'77777'              )    error stop 39
        if ( int_c_short(2)             .ne.  o'0'                  )    error stop 40
        if ( int_c_short(3)             .ne. -32768                 )    error stop 41
        if ( int_c_short(4)             .ne.  32767                 )    error stop 42
        if ( int_c_short(5)             .ne.  b'1111111'            )    error stop 43

        if ( int_c_int(1)               .ne.  2147483647            )    error stop 44
        if ( int_c_int(2)               .ne.  b'1111111'            )    error stop 45
        if ( int_c_int(3)               .ne. -2147483648            )    error stop 46
        if ( int_c_int(4)               .ne.  0             	    )    error stop 47
        if ( int_c_int(5)               .ne.  o'3641100'            )    error stop 48

        if ( ANY (int_c_long(1:5)       .ne.  2147483647            ))   error stop 49

        if ( ANY (int_c_long_long(1:5)  .ne. -9223372036854775807_8 ))   error stop 50

        if ( int_c_size_t(1)            .ne.  2147483647            )    error stop 51
        if ( int_c_size_t(2)            .ne.  b'1111111'            )    error stop 52
        if ( int_c_size_t(3)            .ne. -2147483648            )    error stop 53
        if ( int_c_size_t(4)            .ne.  0            	    )    error stop 54
        if ( int_c_size_t(5)            .ne.  o'3641100'            )    error stop 55

        if ( ANY (int_c_intptr_t(1:5)   .ne.  1000000000            ))   error stop 56

        if ( int_c_intmax_t(1)          .ne.  9223372036854775807_8 )    error stop 57
        if ( int_c_intmax_t(2)          .ne.  b'000000000' 	    )    error stop 58
        if ( int_c_intmax_t(3)          .ne. -9223372036854775807_8 )    error stop 59
        if ( int_c_intmax_t(4)          .ne.  o'3641100'	    )    error stop 60
        if ( int_c_intmax_t(5)          .ne. -2147483648_8 	    )    error stop 61

        if ( ANY (int_c_int8_t(1:5)     .ne. -128                   ))   error stop 62

        if ( int_c_int16_t(1)           .ne.  o'77777'              )    error stop 63
        if ( int_c_int16_t(2)           .ne.  o'0'                  )    error stop 64
        if ( int_c_int16_t(3)           .ne. -32768                 )    error stop 65
        if ( int_c_int16_t(4)           .ne.  32767                 )    error stop 66
        if ( int_c_int16_t(5)           .ne.  b'1111111'            )    error stop 67

        if ( ANY (int_c_int32_t(1:5)    .ne.  2147483647            ))   error stop 68

        if ( ANY (int_c_int64_t(1:5)    .ne.  9223372036850123456_8 ))   error stop 69

        if ( ANY (int_c_int_least8_t(1:5) .ne. -128                 ))   error stop 70

        if ( int_c_int_least16_t(1)   .ne.  o'77777'       	    )    error stop 71
        if ( int_c_int_least16_t(2)   .ne.  o'0'       	    	    )    error stop 72
        if ( int_c_int_least16_t(3)   .ne. -32768       	    )    error stop 73
        if ( int_c_int_least16_t(4)   .ne.  32767              	    )    error stop 74
        if ( int_c_int_least16_t(5)   .ne.  b'1111111'              )    error stop 75

        if ( ANY (int_c_int_least32_t(1:5) .ne.  0                  ))    error stop 76

        if ( ANY (int_c_int_least64_t(1:5) .ne. 1111111111111111111_8 ))  error stop 77

        if ( ANY (int_c_int_fast8_t(1:5)   .ne. b'001'              ))    error stop 78

        if ( int_c_int_fast32_t(1)    .ne.  2147483647              )    error stop 79
        if ( int_c_int_fast32_t(2)    .ne.  b'1111111'              )    error stop 80
        if ( int_c_int_fast32_t(3)    .ne. -2147483648              )    error stop 81
        if ( int_c_int_fast32_t(4)    .ne.  0            	    )    error stop 82
        if ( int_c_int_fast32_t(5)    .ne.  o'3641100'              )    error stop 83

        if ( int_c_int_fast64_t(1)    .ne.  9223372036854775807_8   )    error stop 84
        if ( int_c_int_fast64_t(2)    .ne.  b'000000000'   	    )    error stop 85
        if ( int_c_int_fast64_t(3)    .ne. -9223372036854775807_8   )    error stop 86
        if ( int_c_int_fast64_t(4)    .ne.  o'3641100'   	    )    error stop 87
        if ( int_c_int_fast64_t(5)    .ne. -2147483648_8   	    )    error stop 88



! ----------------------------------------------------------------------------
!  Call to C subprogram
! ----------------------------------------------------------------------------
	CALL CSUB_ALL()


! ----------------------------------------------------------------------------
! Integer Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------


        if ( int_s1a(5)         .ne.  b'1111111'                )    error stop 110
        if ( int_s1a(4)         .ne.  o'0'                      )    error stop 111
        if ( int_s1a(3)         .ne.  -128                      )    error stop 112
        if ( int_s1a(2)         .ne.  o'177'                    )    error stop 113
        if ( int_s1a(1)         .ne.  127                       )    error stop 114

        if ( ANY (int_s1b(1:5)  .ne. 127                       ))   error stop 115

        if ( int_s2a(5)         .ne.  o'77777'                  )    error stop 116
        if ( int_s2a(4)         .ne.  o'0'                      )    error stop 117
        if ( int_s2a(3)         .ne.  -32768                    )    error stop 118
        if ( int_s2a(2)         .ne.  32767                     )    error stop 119
        if ( int_s2a(1)         .ne.  b'1111111'                )    error stop 120

        if ( ANY (int_s2b(1:5)  .ne. 32767                     ))   error stop 121

        if ( int_s4a(5)         .ne.  2147483647                )    error stop 122
        if ( int_s4a(4)         .ne.  b'1111111'                )    error stop 123
        if ( int_s4a(3)         .ne. -2147483648                )    error stop 124
        if ( int_s4a(2)         .ne.  0                         )    error stop 125
        if ( int_s4a(1)         .ne.  o'3641100'                )    error stop 126

        if ( ANY (int_s4b(1:5)  .ne. 2147483647                ))   error stop 127

        if ( int_s8a(5)         .ne.  9223372036854775807_8     )    error stop 128
        if ( int_s8a(4)         .ne.  b'000000000'              )    error stop 129
        if ( int_s8a(3)         .ne. -9223372036854775807_8     )    error stop 130
        if ( int_s8a(2)         .ne.  o'3641100'                )    error stop 131
        if ( int_s8a(1)         .ne. -2147483648_8              )    error stop 132

        if ( ANY (int_s8b(1:5)  .ne. 9223372036854775807_8     ))   error stop 133

        if ( int_c_signed_char(5)       .ne.  b'1111111'            )    error stop 134
        if ( int_c_signed_char(4)       .ne.  o'0'                  )    error stop 135
        if ( int_c_signed_char(3)       .ne. -128                   )    error stop 136
        if ( int_c_signed_char(2)       .ne.  o'177'                )    error stop 137
        if ( int_c_signed_char(1)       .ne.  127                   )    error stop 138

        if ( int_c_short(5)             .ne.  o'77777'              )    error stop 139
        if ( int_c_short(4)             .ne.  o'0'                  )    error stop 140
        if ( int_c_short(3)             .ne. -32768                 )    error stop 141
        if ( int_c_short(2)             .ne.  32767                 )    error stop 142
        if ( int_c_short(1)             .ne.  b'1111111'            )    error stop 143

        if ( int_c_int(5)               .ne.  2147483647            )    error stop 144
        if ( int_c_int(4)               .ne.  b'1111111'            )    error stop 145
        if ( int_c_int(3)               .ne. -2147483648            )    error stop 146
        if ( int_c_int(2)               .ne.  0                     )    error stop 147
        if ( int_c_int(1)               .ne.  o'3641100'            )    error stop 148

        if ( ANY (int_c_long(1:5)       .ne. -2147483648            ))   error stop 149

        if ( ANY (int_c_long_long(1:5)  .ne. -9223372036854775807_8 ))   error stop 150

        if ( int_c_size_t(5)            .ne.  2147483647            )    error stop 151
        if ( int_c_size_t(4)            .ne.  b'1111111'            )    error stop 152
        if ( int_c_size_t(3)            .ne. -2147483648            )    error stop 153
        if ( int_c_size_t(2)            .ne.  0                     )    error stop 154
        if ( int_c_size_t(1)            .ne.  o'3641100'            )    error stop 155

        if ( ANY (int_c_intptr_t(1:5)   .ne.  -1000000000           ))   error stop 156

        if ( int_c_intmax_t(5)          .ne.  9223372036854775807_8 )    error stop 157
        if ( int_c_intmax_t(4)          .ne.  b'000000000'          )    error stop 158
        if ( int_c_intmax_t(3)          .ne. -9223372036854775807_8 )    error stop 159
        if ( int_c_intmax_t(2)          .ne.  o'3641100'            )    error stop 160
        if ( int_c_intmax_t(1)          .ne. -2147483648_8          )    error stop 161

        if ( ANY (int_c_int8_t(1:5)     .ne. 127                   ))   error stop 162

        if ( int_c_int16_t(5)           .ne.  o'77777'              )    error stop 163
        if ( int_c_int16_t(4)           .ne.  o'0'                  )    error stop 164
        if ( int_c_int16_t(3)           .ne. -32768                 )    error stop 165
        if ( int_c_int16_t(2)           .ne.  32767                 )    error stop 166
        if ( int_c_int16_t(1)           .ne.  b'1111111'            )    error stop 167

        if ( ANY (int_c_int32_t(1:5)    .ne. -2147483648            ))   error stop 168

        if ( ANY (int_c_int64_t(1:5)    .ne. -9223372036850123456_8 ))   error stop 169

        if ( ANY (int_c_int_least8_t(1:5) .ne. 127                 ))   error stop 170

        if ( int_c_int_least16_t(5)   .ne.  o'77777'                )    error stop 171
        if ( int_c_int_least16_t(4)   .ne.  o'0'                    )    error stop 172
        if ( int_c_int_least16_t(3)   .ne. -32768                   )    error stop 173
        if ( int_c_int_least16_t(2)   .ne.  32767                   )    error stop 174
        if ( int_c_int_least16_t(1)   .ne.  b'1111111'              )    error stop 175

        if ( ANY (int_c_int_least32_t(1:5) .ne.  2147483647         ))   error stop 176

        if ( ANY (int_c_int_least64_t(1:5) .ne. -111111111111111111_8 )) error stop 177

        if ( ANY (int_c_int_fast8_t(1:5)   .ne. -127                ))   error stop 178

        if ( int_c_int_fast32_t(5)    .ne.  2147483647              )    error stop 179
        if ( int_c_int_fast32_t(4)    .ne.  b'1111111'              )    error stop 180
        if ( int_c_int_fast32_t(3)    .ne. -2147483648              )    error stop 181
        if ( int_c_int_fast32_t(2)    .ne.  0                       )    error stop 182
        if ( int_c_int_fast32_t(1)    .ne.  o'3641100'              )    error stop 183

        if ( int_c_int_fast64_t(5)    .ne.  9223372036854775807_8   )    error stop 184
        if ( int_c_int_fast64_t(4)    .ne.  b'000000000'            )    error stop 185
        if ( int_c_int_fast64_t(3)    .ne. -9223372036854775807_8   )    error stop 186
        if ( int_c_int_fast64_t(2)    .ne.  o'3641100'              )    error stop 187
        if ( int_c_int_fast64_t(1)    .ne. -2147483648_8            )    error stop 188

end program
