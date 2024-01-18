!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk548a cxcmn_blk508
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f fxcmn_blk548a.o cxcmn_blk508.o fxcmn_blk548a
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
!*  DRIVER STANZA              : xlf95, xlc, gcc
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that 1-dimensional array variables
!*				 of all data types inside of common blocks are
!*				 interoperable with C variables 
!*
!*				 Test:  BIND(C) statement in module procedure
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
!*					

module fmod1 
   implicit none

   CONTAINS
     subroutine Intern_FSub()
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16
 
        integer i

        integer (kind=o'001')                           :: int_s1a(3)
        integer (LEN('k'))                              :: int_s1b(3)

        integer (2 )                                    :: int_s2a(3)
        integer (kind=2_4)                              :: int_s2b(3)

        integer  		                        :: int_s4a(3)
        integer (kind=SELECTED_INT_KIND(9))             :: int_s4b(3)

        integer (kind=MAX(8, 7))                        :: int_s8a(3)
        integer (kind=int((4.4e0_8,6.5e0_8))+4 )        :: int_s8b(3)

        INTEGER (C_SIGNED_CHAR          )               :: int_C_SIGNED_CHAR(3)
        INTEGER (C_SHORT                )               :: int_C_SHORT(3)
        INTEGER (C_INT                  )               :: int_C_INT(3)
        INTEGER (C_LONG                 )               :: int_C_LONG(3)
        INTEGER (C_LONG_LONG            )               :: int_C_LONG_LONG(3)
        INTEGER (C_SIZE_T               )               :: int_C_SIZE_T(3)
        INTEGER (C_INTPTR_T             )               :: int_C_INTPTR_T(3)
        INTEGER (C_INTMAX_T             )               :: int_C_INTMAX_T(3)
        INTEGER (C_INT8_T               )               :: int_C_INT8_T(3)
        INTEGER (C_INT16_T              )               :: int_C_INT16_T(3)
        INTEGER (C_INT32_T              )               :: int_C_INT32_T(3)
        INTEGER (C_INT64_T              )               :: int_C_INT64_T(3)
        INTEGER (C_INT_LEAST8_T         )               :: int_C_INT_LEAST8_T(3)
        INTEGER (C_INT_LEAST16_T        )               :: int_C_INT_LEAST16_T(3)
        INTEGER (C_INT_LEAST32_T        )               :: int_C_INT_LEAST32_T(3)
        INTEGER (C_INT_LEAST64_T        )               :: int_C_INT_LEAST64_T(3)
        INTEGER (C_INT_FAST8_T          )               :: int_C_INT_FAST8_T(3)
        INTEGER (C_INT_FAST16_T         )               :: int_C_INT_FAST16_T(3)
        INTEGER (C_INT_FAST32_T         )               :: int_C_INT_FAST32_T(3)
        INTEGER (C_INT_FAST64_T         )               :: int_C_INT_FAST64_T(3)


        real (kind=o'004')                      	:: real_s4a(3)
        real (LEN('Kobi'))                      	:: real_s4b(3)
        real                                    	:: real_s4c(3)
        real (  4)                              	:: real_s4d(3)

        real (kind=MAX(8, 7))                   	:: real_s8a(3)
        real (kind=INT((4.4e0_8,6.5e0_8))+4 )   	:: real_s8b(3)
        real ( 8 )                              	:: real_s8c(3)
        real (KIND=O'010')      			:: real_s8d(3)

                                     	
                            	
        REAL (C_LONG_DOUBLE             )                  	:: real_s16c(3)
                               	

        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4a(3)
        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4b(3)
        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4c(3)
        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4d(3)

        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8a(3)
        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8b(3)
        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8c(3)
        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8d(3)

        character  					:: char1(3)
        character(kind=C_CHAR)  			:: char_C_CHAR(3)

        logical (C_BOOL)  				:: log1(3)


! ----------------------------------------------------------------------------
!      COMMON statement with one COMMON block containing all data types;
!      BIND(C) statement with one bind entity
! ----------------------------------------------------------------------------

        common /Blk_All/ int_C_INT_LEAST32_T  	,  &
                        r_C_DOUBLE_s8a		,  &
                        int_s2b			,  &
                        int_C_INT_FAST32_T	,  &
                        int_s4a			,  &
                        int_C_INT_FAST64_T	,  &
                        real_s16c		,  &
                        int_s4b			,  &
                        int_s8b			,  &
                        int_C_SIGNED_CHAR	,  &
                        int_C_SHORT		,  &
                        real_s8b		,  &
                        int_C_INT		,  &
                        int_C_LONG_LONG		,  &
                        int_C_LONG		,  &
                        char_C_CHAR 		,  &
                        int_C_SIZE_T		,  &
                        int_C_INTPTR_T		,  &
                        real_s4c		,  &
                        int_C_INTMAX_T		,  &
                        int_C_INT8_T		,  &
                        int_C_INT16_T		,  &
                        int_C_INT32_T		,  &
                        r_C_FLOAT_s4a		,  &
                        int_C_INT64_T		,  &
                        int_C_INT_LEAST8_T	,  &
                        int_C_INT_LEAST16_T	,  &
                        char1			,  &
                        int_C_INT_LEAST64_T	,  &
                        log1			,  &
                        int_C_INT_FAST8_T	,  &
			int_s1b			,  &
                        int_C_INT_FAST16_T	,  &
                        real_s4b		

        bind(c) :: /Blk_All/


! ----------------------------------------------------------------------------
! Initialization
! ----------------------------------------------------------------------------

	int_C_INT_LEAST32_T 		=   50
        r_C_DOUBLE_s8a     		=   51
        int_s2b            		=   52
        int_C_INT_FAST32_T 		=   53
        int_s4a            		=   54
        int_C_INT_FAST64_T 		=   55
        real_s16c         		=   56
        int_s4b          		=   57
        int_s8b         		=   58
        int_C_SIGNED_CHAR     		=   59
        int_C_SHORT          		=   60
        real_s8b    			=   61
        int_C_INT           		=   62
        int_C_LONG_LONG    		=   63
        int_C_LONG        		=   64
        char_C_CHAR      		=   'A'
        int_C_SIZE_T    		=   65
        int_C_INTPTR_T 			=   66
        real_s4c      			=   67
        int_C_INTMAX_T			=   68
        int_C_INT8_T         		=   69
        int_C_INT16_T       		=   70
        int_C_INT32_T      		=   71
        r_C_FLOAT_s4a     		=   72
        int_C_INT64_T    		=   73
        int_C_INT_LEAST8_T 		=   74
        int_C_INT_LEAST16_T		=   75
        char1            		=   'A'
        int_C_INT_LEAST64_T  		=   76
        log1               		=  .true.
        int_C_INT_FAST8_T		=   77
        int_s1b         		=   78
        int_C_INT_FAST16_T    		=   79
        real_s4b			=   80


! ----------------------------------------------------------------------------
! Verification
!       - verify assigned values before passing to C
! ----------------------------------------------------------------------------

      do i = 1, 3

        if (         int_C_INT_LEAST32_T(i)             .ne.   50 )      error stop 5
        if (         r_C_DOUBLE_s8a(i)                  .ne.   51 )      error stop 6
        if (         int_s2b(i)                         .ne.   52 )      error stop 7
        if (         int_C_INT_FAST32_T(i)              .ne.   53 )      error stop 8
        if (         int_s4a(i)                         .ne.   54 )      error stop 9
        if (         int_C_INT_FAST64_T(i)              .ne.   55 )      error stop 10
        if (         real_s16c(i)                       .ne.   56 )      error stop 11
        if (         int_s4b(i)                         .ne.   57 )      error stop 22
        if (         int_s8b(i)                         .ne.   58 )      error stop 23
        if (         int_C_SIGNED_CHAR(i)               .ne.   59 )      error stop 24
        if (         int_C_SHORT(i)                     .ne.   60 )      error stop 25
        if (         real_s8b(i)                        .ne.   61 )      error stop 26
        if (         int_C_INT(i)                       .ne.   62 )      error stop 27
        if (         int_C_LONG_LONG(i)                 .ne.   63 )      error stop 28
        if (         int_C_LONG(i)                      .ne.   64 )      error stop 29
        if (         char_C_CHAR(i)                     .ne.   'A')      error stop 20
        if (         int_C_SIZE_T(i)                    .ne.   65 )      error stop 21
        if (         int_C_INTPTR_T(i)                  .ne.   66 )      error stop 32
        if (         real_s4c(i)                        .ne.   67 )      error stop 33
        if (         int_C_INTMAX_T(i)                  .ne.   68 )      error stop 33
        if (         int_C_INT8_T(i)                    .ne.   69 )      error stop 34
        if (         int_C_INT16_T(i)                   .ne.   70 )      error stop 35
        if (         int_C_INT32_T(i)                   .ne.   71 )      error stop 36
        if (         r_C_FLOAT_s4a(i)                   .ne.   72 )      error stop 37
        if (         int_C_INT64_T(i)                   .ne.   73 )      error stop 38
        if (         int_C_INT_LEAST8_T(i)              .ne.   74 )      error stop 39
        if (         int_C_INT_LEAST16_T(i)             .ne.   75 )      error stop 40
        if (         char1(i)                           .ne.   'A')      error stop 41
        if (         int_C_INT_LEAST64_T(i)             .ne.   76 )      error stop 42
        if (         log1(i)                          .neqv.  .true. )   error stop 43
        if (         int_C_INT_FAST8_T(i)               .ne.   77 )      error stop 44
        if (         int_s1b(i)                         .ne.   78 )      error stop 45
        if (         int_C_INT_FAST16_T(i)              .ne.   79 )      error stop 46
        if (         real_s4b(i)                        .ne.   80 )      error stop 47

      end do 




! ----------------------------------------------------------------------------
!  Call to C subprogram
! ----------------------------------------------------------------------------
        CALL CSUB_ALL()


! ----------------------------------------------------------------------------
!  Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------

      do i = 1, 3

        if ( int_c_int_least32_t(i)    .ne. 80 )      error stop 50
        if ( r_c_double_s8a(i)         .ne. 79 )      error stop 51
        if ( int_s2b(i)                .ne. 78 )      error stop 52
        if ( int_c_int_fast32_t(i)     .ne. 77 )      error stop 53
        if ( int_s4a(i)                .ne. 76 )      error stop 54
        if ( int_c_int_fast64_t(i)     .ne. 75 )      error stop 55
        if ( real_s16c(i)              .ne. 74 )      error stop 56
        if ( int_s4b(i)                .ne. 73 )      error stop 57
        if ( int_s8b(i)                .ne. 72 )      error stop 58
        if ( int_c_signed_char(i)      .ne. 71 )      error stop 59
        if ( int_c_short(i)            .ne. 70 )      error stop 60
        if ( real_s8b(i)               .ne. 69 )      error stop 61
        if ( int_c_int(i)              .ne. 68 )      error stop 62
        if ( int_c_long_long(i)        .ne. 67 )      error stop 63
        if ( int_c_long(i)             .ne. 66 )      error stop 64
        if ( char_c_char(i)            .ne. 'Z' )     error stop 65
        if ( int_c_size_t(i)           .ne. 65 )      error stop 66
        if ( int_c_intptr_t(i)         .ne. 64 )      error stop 67
        if ( real_s4c(i)               .ne. 63 )      error stop 68
        if ( int_c_intmax_t(i)         .ne. 62 )      error stop 69
        if ( int_c_int8_t(i)           .ne. 61 )      error stop 70
        if ( int_c_int16_t(i)          .ne. 60 )      error stop 71
        if ( int_c_int32_t(i)          .ne. 59 )      error stop 72
        if ( r_c_float_s4a(i)          .ne. 58 )      error stop 73
        if ( int_c_int64_t(i)          .ne. 57 )      error stop 74
        if ( int_c_int_least8_t(i)     .ne. 56 )      error stop 75
        if ( int_c_int_least16_t(i)    .ne. 55 )      error stop 76
        if ( char1(i)                  .ne. 'Z' )     error stop 77
        if ( int_c_int_least64_t(i)    .ne. 54 )      error stop 78
        if ( log1(i)               .neqv. .false. )   error stop 79
        if ( int_c_int_fast8_t(i)      .ne. 53 )      error stop 80
        if ( int_s1b(i)                .ne. 52 )      error stop 81
        if ( int_c_int_fast16_t(i)     .ne. 51 )      error stop 82
        if ( real_s4b(i)               .ne. 50 )      error stop 83
      end do 


     end subroutine

end module fmod1 



program fxcmn_blk548a
	use fmod1 
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16
 
        integer i

        integer (kind=o'001')                           :: int_s1a(3)
        integer (LEN('k'))                              :: int_s1b(3)

        integer (2 )                                    :: int_s2a(3)
        integer (kind=2_4)                              :: int_s2b(3)

        integer  		                        :: int_s4a(3)
        integer (kind=SELECTED_INT_KIND(9))             :: int_s4b(3)

        integer (kind=MAX(8, 7))                        :: int_s8a(3)
        integer (kind=int((4.4e0_8,6.5e0_8))+4 )        :: int_s8b(3)

        INTEGER (C_SIGNED_CHAR          )               :: int_C_SIGNED_CHAR(3)
        INTEGER (C_SHORT                )               :: int_C_SHORT(3)
        INTEGER (C_INT                  )               :: int_C_INT(3)
        INTEGER (C_LONG                 )               :: int_C_LONG(3)
        INTEGER (C_LONG_LONG            )               :: int_C_LONG_LONG(3)
        INTEGER (C_SIZE_T               )               :: int_C_SIZE_T(3)
        INTEGER (C_INTPTR_T             )               :: int_C_INTPTR_T(3)
        INTEGER (C_INTMAX_T             )               :: int_C_INTMAX_T(3)
        INTEGER (C_INT8_T               )               :: int_C_INT8_T(3)
        INTEGER (C_INT16_T              )               :: int_C_INT16_T(3)
        INTEGER (C_INT32_T              )               :: int_C_INT32_T(3)
        INTEGER (C_INT64_T              )               :: int_C_INT64_T(3)
        INTEGER (C_INT_LEAST8_T         )               :: int_C_INT_LEAST8_T(3)
        INTEGER (C_INT_LEAST16_T        )               :: int_C_INT_LEAST16_T(3)
        INTEGER (C_INT_LEAST32_T        )               :: int_C_INT_LEAST32_T(3)
        INTEGER (C_INT_LEAST64_T        )               :: int_C_INT_LEAST64_T(3)
        INTEGER (C_INT_FAST8_T          )               :: int_C_INT_FAST8_T(3)
        INTEGER (C_INT_FAST16_T         )               :: int_C_INT_FAST16_T(3)
        INTEGER (C_INT_FAST32_T         )               :: int_C_INT_FAST32_T(3)
        INTEGER (C_INT_FAST64_T         )               :: int_C_INT_FAST64_T(3)


        real (kind=o'004')                      	:: real_s4a(3)
        real (LEN('Kobi'))                      	:: real_s4b(3)
        real                                    	:: real_s4c(3)
        real (  4)                              	:: real_s4d(3)

        real (kind=MAX(8, 7))                   	:: real_s8a(3)
        real (kind=INT((4.4e0_8,6.5e0_8))+4 )   	:: real_s8b(3)
        real ( 8 )                              	:: real_s8c(3)
        real (KIND=O'010')      			:: real_s8d(3)

                                     	
                            	
        REAL (C_LONG_DOUBLE             )                  	:: real_s16c(3)
                               	

        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4a(3)
        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4b(3)
        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4c(3)
        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4d(3)

        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8a(3)
        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8b(3)
        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8c(3)
        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8d(3)

        character  					:: char1(3)
        character(kind=C_CHAR)  			:: char_C_CHAR(3)

        logical (C_BOOL)  				:: log1(3)


! ----------------------------------------------------------------------------
!      COMMON statement with one COMMON block containing all data types;
!      BIND(C) statement with one bind entity
! ----------------------------------------------------------------------------

        common /Blk_All/ int_C_INT_LEAST32_T  	,  &
                        r_C_DOUBLE_s8a		,  &
                        int_s2b			,  &
                        int_C_INT_FAST32_T	,  &
                        int_s4a			,  &
                        int_C_INT_FAST64_T	,  &
                        real_s16c		,  &
                        int_s4b			,  &
                        int_s8b			,  &
                        int_C_SIGNED_CHAR	,  &
                        int_C_SHORT		,  &
                        real_s8b		,  &
                        int_C_INT		,  &
                        int_C_LONG_LONG		,  &
                        int_C_LONG		,  &
                        char_C_CHAR 		,  &
                        int_C_SIZE_T		,  &
                        int_C_INTPTR_T		,  &
                        real_s4c		,  &
                        int_C_INTMAX_T		,  &
                        int_C_INT8_T		,  &
                        int_C_INT16_T		,  &
                        int_C_INT32_T		,  &
                        r_C_FLOAT_s4a		,  &
                        int_C_INT64_T		,  &
                        int_C_INT_LEAST8_T	,  &
                        int_C_INT_LEAST16_T	,  &
                        char1			,  &
                        int_C_INT_LEAST64_T	,  &
                        log1			,  &
                        int_C_INT_FAST8_T	,  &
			int_s1b			,  &
                        int_C_INT_FAST16_T	,  &
                        real_s4b		

        bind(c) :: /Blk_All/

	!*** Call module subroutine
	call Intern_FSub()

! ---------------------------------------------------------------------------- 
! Verification 
!       - verify values passed back from module subroutine
! ----------------------------------------------------------------------------

      do i = 1, 3

        if ( int_c_int_least32_t(i)    .ne. 80 )      error stop 170
        if ( r_c_double_s8a(i)         .ne. 79 )      error stop 171
        if ( int_s2b(i)                .ne. 78 )      error stop 172
        if ( int_c_int_fast32_t(i)     .ne. 77 )      error stop 173
        if ( int_s4a(i)                .ne. 76 )      error stop 174
        if ( int_c_int_fast64_t(i)     .ne. 75 )      error stop 175
        if ( real_s16c(i)              .ne. 74 )      error stop 176
        if ( int_s4b(i)                .ne. 73 )      error stop 177
        if ( int_s8b(i)                .ne. 72 )      error stop 178
        if ( int_c_signed_char(i)      .ne. 71 )      error stop 179
        if ( int_c_short(i)            .ne. 70 )      error stop 180
        if ( real_s8b(i)               .ne. 69 )      error stop 181
        if ( int_c_int(i)              .ne. 68 )      error stop 182
        if ( int_c_long_long(i)        .ne. 67 )      error stop 183
        if ( int_c_long(i)             .ne. 66 )      error stop 184
        if ( char_c_char(i)            .ne. 'Z' )     error stop 185
        if ( int_c_size_t(i)           .ne. 65 )      error stop 186
        if ( int_c_intptr_t(i)         .ne. 64 )      error stop 187
        if ( real_s4c(i)               .ne. 63 )      error stop 188
        if ( int_c_intmax_t(i)         .ne. 62 )      error stop 189
        if ( int_c_int8_t(i)           .ne. 61 )      error stop 190
        if ( int_c_int16_t(i)          .ne. 60 )      error stop 191
        if ( int_c_int32_t(i)          .ne. 59 )      error stop 192
        if ( r_c_float_s4a(i)          .ne. 58 )      error stop 193
        if ( int_c_int64_t(i)          .ne. 57 )      error stop 194
        if ( int_c_int_least8_t(i)     .ne. 56 )      error stop 195
        if ( int_c_int_least16_t(i)    .ne. 55 )      error stop 196
        if ( char1(i)                  .ne. 'Z' )     error stop 197
        if ( int_c_int_least64_t(i)    .ne. 54 )      error stop 198
        if ( log1(i)               .neqv. .false. )   error stop 199

        if ( int_c_int_fast8_t(i)      .ne. 53 )      error stop 130
        if ( int_s1b(i)                .ne. 52 )      error stop 131
        if ( int_c_int_fast16_t(i)     .ne. 51 )      error stop 132
        if ( real_s4b(i)               .ne. 50 )      error stop 133

      end do 


end program
