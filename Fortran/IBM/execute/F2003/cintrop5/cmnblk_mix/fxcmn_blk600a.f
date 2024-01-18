!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk600a cxcmn_blk550
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:  rm -f fxcmn_blk600a.o cxcmn_blk550.o fxcmn_blk600a
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
!*  DESCRIPTION                : This test case will verify that derived type with 3-dimensional
!*				 array variables of all data types inside of common blocks are
!*				 interoperable with C variables 
!*
!*				 Test:  BIND(C) statement in internal subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk600a
	use iso_c_binding
        implicit none

	call intern_fsub()

   CONTAINS

   subroutine intern_fsub()
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16
 
        integer i,j,k

      type, bind(c)  :: dt

        INTEGER (C_INT_LEAST32_T        )               :: int_C_INT_LEAST32_T(2,2,2)
        REAL (C_DOUBLE                  )               :: r_C_DOUBLE_s8a(2,2,2)
        integer (kind=2_4)                              :: int_s2b(2,2,2)
        INTEGER (C_INT_FAST32_T         )               :: int_C_INT_FAST32_T(2,2,2)
        integer                                         :: int_s4a(2,2,2)
        INTEGER (C_INT_FAST64_T         )               :: int_C_INT_FAST64_T(2,2,2)
        REAL (C_LONG_DOUBLE             )               :: real_s16c(2,2,2)
        integer (kind=SELECTED_INT_KIND(9))             :: int_s4b(2,2,2)
        integer (kind=int((4.4e0_8,6.5e0_8))+4 )        :: int_s8b(2,2,2)
        INTEGER (C_SIGNED_CHAR          )               :: int_C_SIGNED_CHAR(2,2,2)
        INTEGER (C_SHORT                )               :: int_C_SHORT(2,2,2)
        real (kind=INT((4.4e0_8,6.5e0_8))+4 )           :: real_s8b(2,2,2)
        INTEGER (C_INT                  )               :: int_C_INT(2,2,2)
        INTEGER (C_LONG_LONG            )               :: int_C_LONG_LONG(2,2,2)
        INTEGER (C_LONG                 )               :: int_C_LONG(2,2,2)
        character(kind=C_CHAR)                          :: char_C_CHAR(2,2,2)
        INTEGER (C_SIZE_T               )               :: int_C_SIZE_T(2,2,2)
        INTEGER (C_INTPTR_T             )               :: int_C_INTPTR_T(2,2,2)
        real                                            :: real_s4c(2,2,2)
        INTEGER (C_INTMAX_T             )               :: int_C_INTMAX_T(2,2,2)
        INTEGER (C_INT8_T               )               :: int_C_INT8_T(2,2,2)
        INTEGER (C_INT16_T              )               :: int_C_INT16_T(2,2,2)
        INTEGER (C_INT32_T              )               :: int_C_INT32_T(2,2,2)
        REAL (C_FLOAT                   )               :: r_C_FLOAT_s4a(2,2,2)
        INTEGER (C_INT64_T              )               :: int_C_INT64_T(2,2,2)
        INTEGER (C_INT_LEAST8_T         )               :: int_C_INT_LEAST8_T(2,2,2)
        INTEGER (C_INT_LEAST16_T        )               :: int_C_INT_LEAST16_T(2,2,2)
        character                                       :: char1(2,2,2)
        INTEGER (C_INT_LEAST64_T        )               :: int_C_INT_LEAST64_T(2,2,2)
        logical (C_BOOL)                                :: log1(2,2,2)
        INTEGER (C_INT_FAST8_T          )               :: int_C_INT_FAST8_T(2,2,2)
        integer (LEN('k'))                              :: int_s1b(2,2,2)
        INTEGER (C_INT_FAST16_T         )               :: int_C_INT_FAST16_T(2,2,2)
        real (LEN('Kobi'))                              :: real_s4b(2,2,2)

      end type dt

      type(dt) :: dt_d



! ----------------------------------------------------------------------------
!      COMMON statement with one COMMON block containing all data types inside a derived type;
!      BIND(C) statement with one bind entity
! ----------------------------------------------------------------------------

        common /Blk_All/ dt_d


        bind(c) :: /Blk_All/


! ----------------------------------------------------------------------------
! Initialization
! ----------------------------------------------------------------------------

	dt_d%int_C_INT_LEAST32_T 		=   50
        dt_d%r_C_DOUBLE_s8a     		=   51
        dt_d%int_s2b            		=   52
        dt_d%int_C_INT_FAST32_T 		=   53
        dt_d%int_s4a            		=   54
        dt_d%int_C_INT_FAST64_T 		=   55
        dt_d%real_s16c         			=   56
        dt_d%int_s4b          			=   57
        dt_d%int_s8b         			=   58
        dt_d%int_C_SIGNED_CHAR     		=   59
        dt_d%int_C_SHORT          		=   60
        dt_d%real_s8b    			=   61
        dt_d%int_C_INT           		=   62
        dt_d%int_C_LONG_LONG    		=   63
        dt_d%int_C_LONG        			=   64
        dt_d%char_C_CHAR      			=   'A'
        dt_d%int_C_SIZE_T    			=   65
        dt_d%int_C_INTPTR_T 			=   66
        dt_d%real_s4c      			=   67
        dt_d%int_C_INTMAX_T			=   68
        dt_d%int_C_INT8_T         		=   69
        dt_d%int_C_INT16_T       		=   70
        dt_d%int_C_INT32_T      		=   71
        dt_d%r_C_FLOAT_s4a     			=   72
        dt_d%int_C_INT64_T    			=   73
        dt_d%int_C_INT_LEAST8_T 		=   74
        dt_d%int_C_INT_LEAST16_T		=   75
        dt_d%char1            			=   'A'
        dt_d%int_C_INT_LEAST64_T  		=   76
        dt_d%log1               		=  .true.
        dt_d%int_C_INT_FAST8_T			=   77
        dt_d%int_s1b         			=   78
        dt_d%int_C_INT_FAST16_T    		=   79
        dt_d%real_s4b				=   80


! ----------------------------------------------------------------------------
! Verification
!       - verify assigned values before passing to C
! ----------------------------------------------------------------------------

      do i = 1, 2
        do j = 1, 2
          do k = 1, 2

        if (  dt_d%int_C_INT_LEAST32_T(k,j,i)             .ne.   50 )      error stop 5
        if (  dt_d%r_C_DOUBLE_s8a(k,j,i)                  .ne.   51 )      error stop 6
        if (  dt_d%int_s2b(k,j,i)                         .ne.   52 )      error stop 7
        if (  dt_d%int_C_INT_FAST32_T(k,j,i)              .ne.   53 )      error stop 8
        if (  dt_d%int_s4a(k,j,i)                         .ne.   54 )      error stop 9
        if (  dt_d%int_C_INT_FAST64_T(k,j,i)              .ne.   55 )      error stop 10
        if (  dt_d%real_s16c(k,j,i)                       .ne.   56 )      error stop 11
        if (  dt_d%int_s4b(k,j,i)                         .ne.   57 )      error stop 22
        if (  dt_d%int_s8b(k,j,i)                         .ne.   58 )      error stop 23
        if (  dt_d%int_C_SIGNED_CHAR(k,j,i)               .ne.   59 )      error stop 24
        if (  dt_d%int_C_SHORT(k,j,i)                     .ne.   60 )      error stop 25
        if (  dt_d%real_s8b(k,j,i)                        .ne.   61 )      error stop 26
        if (  dt_d%int_C_INT(k,j,i)                       .ne.   62 )      error stop 27
        if (  dt_d%int_C_LONG_LONG(k,j,i)                 .ne.   63 )      error stop 28
        if (  dt_d%int_C_LONG(k,j,i)                      .ne.   64 )      error stop 29
        if (  dt_d%char_C_CHAR(k,j,i)                     .ne.   'A')      error stop 20
        if (  dt_d%int_C_SIZE_T(k,j,i)                    .ne.   65 )      error stop 21
        if (  dt_d%int_C_INTPTR_T(k,j,i)                  .ne.   66 )      error stop 32
        if (  dt_d%real_s4c(k,j,i)                        .ne.   67 )      error stop 33
        if (  dt_d%int_C_INTMAX_T(k,j,i)                  .ne.   68 )      error stop 33
        if (  dt_d%int_C_INT8_T(k,j,i)                    .ne.   69 )      error stop 34
        if (  dt_d%int_C_INT16_T(k,j,i)                   .ne.   70 )      error stop 35
        if (  dt_d%int_C_INT32_T(k,j,i)                   .ne.   71 )      error stop 36
        if (  dt_d%r_C_FLOAT_s4a(k,j,i)                   .ne.   72 )      error stop 37
        if (  dt_d%int_C_INT64_T(k,j,i)                   .ne.   73 )      error stop 38
        if (  dt_d%int_C_INT_LEAST8_T(k,j,i)              .ne.   74 )      error stop 39
        if (  dt_d%int_C_INT_LEAST16_T(k,j,i)             .ne.   75 )      error stop 40
        if (  dt_d%char1(k,j,i)                           .ne.   'A')      error stop 41
        if (  dt_d%int_C_INT_LEAST64_T(k,j,i)             .ne.   76 )      error stop 42
        if (  dt_d%log1(k,j,i)                          .neqv.  .true. )   error stop 43
        if (  dt_d%int_C_INT_FAST8_T(k,j,i)               .ne.   77 )      error stop 44
        if (  dt_d%int_s1b(k,j,i)                         .ne.   78 )      error stop 45
        if (  dt_d%int_C_INT_FAST16_T(k,j,i)              .ne.   79 )      error stop 46
        if (  dt_d%real_s4b(k,j,i)                        .ne.   80 )      error stop 47

          end do
        end do
      end do





! ----------------------------------------------------------------------------
!  Call to C subprogram
! ----------------------------------------------------------------------------
        CALL CSUB_ALL()


! ----------------------------------------------------------------------------
!  Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------

      do i = 1, 2
        do j = 1, 2
          do k = 1, 2


        if (  dt_d%int_c_int_least32_t(k,j,i)    .ne. 80 )      error stop 50
        if (  dt_d%r_c_double_s8a(k,j,i)         .ne. 79 )      error stop 51
        if (  dt_d%int_s2b(k,j,i)                .ne. 78 )      error stop 52
        if (  dt_d%int_c_int_fast32_t(k,j,i)     .ne. 77 )      error stop 53
        if (  dt_d%int_s4a(k,j,i)                .ne. 76 )      error stop 54
        if (  dt_d%int_c_int_fast64_t(k,j,i)     .ne. 75 )      error stop 55
        if (  dt_d%real_s16c(k,j,i)              .ne. 74 )      error stop 56
        if (  dt_d%int_s4b(k,j,i)                .ne. 73 )      error stop 57
        if (  dt_d%int_s8b(k,j,i)                .ne. 72 )      error stop 58
        if (  dt_d%int_c_signed_char(k,j,i)      .ne. 71 )      error stop 59
        if (  dt_d%int_c_short(k,j,i)            .ne. 70 )      error stop 60
        if (  dt_d%real_s8b(k,j,i)               .ne. 69 )      error stop 61
        if (  dt_d%int_c_int(k,j,i)              .ne. 68 )      error stop 62
        if (  dt_d%int_c_long_long(k,j,i)        .ne. 67 )      error stop 63
        if (  dt_d%int_c_long(k,j,i)             .ne. 66 )      error stop 64
        if (  dt_d%char_c_char(k,j,i)            .ne. 'Z' )     error stop 65
        if (  dt_d%int_c_size_t(k,j,i)           .ne. 65 )      error stop 66
        if (  dt_d%int_c_intptr_t(k,j,i)         .ne. 64 )      error stop 67
        if (  dt_d%real_s4c(k,j,i)               .ne. 63 )      error stop 68
        if (  dt_d%int_c_intmax_t(k,j,i)         .ne. 62 )      error stop 69
        if (  dt_d%int_c_int8_t(k,j,i)           .ne. 61 )      error stop 70
        if (  dt_d%int_c_int16_t(k,j,i)          .ne. 60 )      error stop 71
        if (  dt_d%int_c_int32_t(k,j,i)          .ne. 59 )      error stop 72
        if (  dt_d%r_c_float_s4a(k,j,i)          .ne. 58 )      error stop 73
        if (  dt_d%int_c_int64_t(k,j,i)          .ne. 57 )      error stop 74
        if (  dt_d%int_c_int_least8_t(k,j,i)     .ne. 56 )      error stop 75
        if (  dt_d%int_c_int_least16_t(k,j,i)    .ne. 55 )      error stop 76
        if (  dt_d%char1(k,j,i)                  .ne. 'Z' )     error stop 77
        if (  dt_d%int_c_int_least64_t(k,j,i)    .ne. 54 )      error stop 78
        if (  dt_d%log1(k,j,i)               .neqv. .false. )   error stop 79
        if (  dt_d%int_c_int_fast8_t(k,j,i)      .ne. 53 )      error stop 80
        if (  dt_d%int_s1b(k,j,i)                .ne. 52 )      error stop 81
        if (  dt_d%int_c_int_fast16_t(k,j,i)     .ne. 51 )      error stop 82
        if (  dt_d%real_s4b(k,j,i)               .ne. 50 )      error stop 83

          end do
        end do
      end do


   end subroutine

end program
