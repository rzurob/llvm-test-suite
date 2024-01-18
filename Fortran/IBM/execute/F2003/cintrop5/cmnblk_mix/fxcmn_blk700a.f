!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk700a cxcmn_blk700
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f fxcmn_blk700a.o cxcmn_blk5700.o fxcmn_blk700a
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
!*  DESCRIPTION                : This test case will verify that 3-dimensional array variables
!*				 of all data types inside of common blocks are
!*				 interoperable with C variables 
!*
!*				 Test:  BIND(C) statement in main program
!*
!*				 C main program will call the Fortran subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


subroutine fxcmn_blk700a()
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16
 
        integer i,j,k

        integer (kind=o'001')                           :: int_s1a(2,2,2)
        integer (LEN('k'))                              :: int_s1b(2,2,2)

        integer (2 )                                    :: int_s2a(2,2,2)
        integer (kind=2_4)                              :: int_s2b(2,2,2)

        integer  		                        :: int_s4a(2,2,2)
        integer (kind=SELECTED_INT_KIND(9))             :: int_s4b(2,2,2)

        integer (kind=MAX(8, 7))                        :: int_s8a(2,2,2)
        integer (kind=int((4.4e0_8,6.5e0_8))+4 )        :: int_s8b(2,2,2)

        INTEGER (C_SIGNED_CHAR          )               :: int_C_SIGNED_CHAR(2,2,2)
        INTEGER (C_SHORT                )               :: int_C_SHORT(2,2,2)
        INTEGER (C_INT                  )               :: int_C_INT(2,2,2)
        INTEGER (C_LONG                 )               :: int_C_LONG(2,2,2)
        INTEGER (C_LONG_LONG            )               :: int_C_LONG_LONG(2,2,2)
        INTEGER (C_SIZE_T               )               :: int_C_SIZE_T(2,2,2)
        INTEGER (C_INTPTR_T             )               :: int_C_INTPTR_T(2,2,2)
        INTEGER (C_INTMAX_T             )               :: int_C_INTMAX_T(2,2,2)
        INTEGER (C_INT8_T               )               :: int_C_INT8_T(2,2,2)
        INTEGER (C_INT16_T              )               :: int_C_INT16_T(2,2,2)
        INTEGER (C_INT32_T              )               :: int_C_INT32_T(2,2,2)
        INTEGER (C_INT64_T              )               :: int_C_INT64_T(2,2,2)
        INTEGER (C_INT_LEAST8_T         )               :: int_C_INT_LEAST8_T(2,2,2)
        INTEGER (C_INT_LEAST16_T        )               :: int_C_INT_LEAST16_T(2,2,2)
        INTEGER (C_INT_LEAST32_T        )               :: int_C_INT_LEAST32_T(2,2,2)
        INTEGER (C_INT_LEAST64_T        )               :: int_C_INT_LEAST64_T(2,2,2)
        INTEGER (C_INT_FAST8_T          )               :: int_C_INT_FAST8_T(2,2,2)
        INTEGER (C_INT_FAST16_T         )               :: int_C_INT_FAST16_T(2,2,2)
        INTEGER (C_INT_FAST32_T         )               :: int_C_INT_FAST32_T(2,2,2)
        INTEGER (C_INT_FAST64_T         )               :: int_C_INT_FAST64_T(2,2,2)


        real (kind=o'004')                      	:: real_s4a(2,2,2)
        real (LEN('Kobi'))                      	:: real_s4b(2,2,2)
        real                                    	:: real_s4c(2,2,2)
        real (  4)                              	:: real_s4d(2,2,2)

        real (kind=MAX(8, 7))                   	:: real_s8a(2,2,2)
        real (kind=INT((4.4e0_8,6.5e0_8))+4 )   	:: real_s8b(2,2,2)
        real ( 8 )                              	:: real_s8c(2,2,2)
        real (KIND=O'010')      			:: real_s8d(2,2,2)

        REAL (C_LONG_DOUBLE             )              	:: real_s16c(2,2,2)

        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4a(2,2,2)
        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4b(2,2,2)
        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4c(2,2,2)
        REAL (C_FLOAT                   )       	:: r_C_FLOAT_s4d(2,2,2)

        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8a(2,2,2)
        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8b(2,2,2)
        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8c(2,2,2)
        REAL (C_DOUBLE                  )       	:: r_C_DOUBLE_s8d(2,2,2)

        character  					:: char1(2,2,2)
        character(kind=C_CHAR)  			:: char_C_CHAR(2,2,2)

        logical (C_BOOL)  				:: log1(2,2,2)


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
!  Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------

      do i = 1, 2
        do j = 1, 2
          do k = 1, 2


        if ( int_c_int_least32_t(k,j,i)    .ne. 80 )      error stop 50
        if ( r_c_double_s8a(k,j,i)         .ne. 79 )      error stop 51
        if ( int_s2b(k,j,i)                .ne. 78 )      error stop 52
        if ( int_c_int_fast32_t(k,j,i)     .ne. 77 )      error stop 53
        if ( int_s4a(k,j,i)                .ne. 76 )      error stop 54
        if ( int_c_int_fast64_t(k,j,i)     .ne. 75 )      error stop 55
        if ( real_s16c(k,j,i)              .ne. 74 )      error stop 56
        if ( int_s4b(k,j,i)                .ne. 73 )      error stop 57
        if ( int_s8b(k,j,i)                .ne. 72 )      error stop 58
        if ( int_c_signed_char(k,j,i)      .ne. 71 )      error stop 59
        if ( int_c_short(k,j,i)            .ne. 70 )      error stop 60
        if ( real_s8b(k,j,i)               .ne. 69 )      error stop 61
        if ( int_c_int(k,j,i)              .ne. 68 )      error stop 62
        if ( int_c_long_long(k,j,i)        .ne. 67 )      error stop 63
        if ( int_c_long(k,j,i)             .ne. 66 )      error stop 64
        if ( char_c_char(k,j,i)            .ne. 'Z' )     error stop 65
        if ( int_c_size_t(k,j,i)           .ne. 65 )      error stop 66
        if ( int_c_intptr_t(k,j,i)         .ne. 64 )      error stop 67
        if ( real_s4c(k,j,i)               .ne. 63 )      error stop 68
        if ( int_c_intmax_t(k,j,i)         .ne. 62 )      error stop 69
        if ( int_c_int8_t(k,j,i)           .ne. 61 )      error stop 70
        if ( int_c_int16_t(k,j,i)          .ne. 60 )      error stop 71
        if ( int_c_int32_t(k,j,i)          .ne. 59 )      error stop 72
        if ( r_c_float_s4a(k,j,i)          .ne. 58 )      error stop 73
        if ( int_c_int64_t(k,j,i)          .ne. 57 )      error stop 74
        if ( int_c_int_least8_t(k,j,i)     .ne. 56 )      error stop 75
        if ( int_c_int_least16_t(k,j,i)    .ne. 55 )      error stop 76
        if ( char1(k,j,i)                  .ne. 'Z' )     error stop 77
        if ( int_c_int_least64_t(k,j,i)    .ne. 54 )      error stop 78
        if ( log1(k,j,i)               .neqv. .false. )   error stop 79
        if ( int_c_int_fast8_t(k,j,i)      .ne. 53 )      error stop 80
        if ( int_s1b(k,j,i)                .ne. 52 )      error stop 81
        if ( int_c_int_fast16_t(k,j,i)     .ne. 51 )      error stop 82
        if ( real_s4b(k,j,i)               .ne. 50 )      error stop 83

          end do
        end do
      end do


! ----------------------------------------------------------------------------
!  Modification
!       - modify values and pass back to C
! ----------------------------------------------------------------------------

      do i = 1, 2
        do j = 1, 2
          do k = 1, 2

         int_c_int_least32_t(k,j,i)     =  40
         r_c_double_s8a(k,j,i)          =  39
         int_s2b(k,j,i)                 =  38
         int_c_int_fast32_t(k,j,i)      =  37
         int_s4a(k,j,i)                 =  36
         int_c_int_fast64_t(k,j,i)      =  35
         real_s16c(k,j,i)               =  34
         int_s4b(k,j,i)                 =  33
         int_s8b(k,j,i)                 =  32
         int_c_signed_char(k,j,i)       =  31
         int_c_short(k,j,i)             =  30
         real_s8b(k,j,i)                =  29
         int_c_int(k,j,i)               =  28
         int_c_long_long(k,j,i)         =  27
         int_c_long(k,j,i)              =  26
         char_c_char(k,j,i)             =  'a'
         int_c_size_t(k,j,i)            =  25
         int_c_intptr_t(k,j,i)          =  24
         real_s4c(k,j,i)                =  23
         int_c_intmax_t(k,j,i)          =  22
         int_c_int8_t(k,j,i)            =  21
         int_c_int16_t(k,j,i)           =  20
         int_c_int32_t(k,j,i)           =  19
         r_c_float_s4a(k,j,i)           =  18
         int_c_int64_t(k,j,i)           =  17
         int_c_int_least8_t(k,j,i)      =  16
         int_c_int_least16_t(k,j,i)     =  15
         char1(k,j,i)                   =  'b'
         int_c_int_least64_t(k,j,i)     =  14
         log1(k,j,i)                    =  .true.
         int_c_int_fast8_t(k,j,i)       =  13
         int_s1b(k,j,i)                 =  12
         int_c_int_fast16_t(k,j,i)      =  11
         real_s4b(k,j,i)                =  10.

          end do
        end do
      end do


end subroutine

