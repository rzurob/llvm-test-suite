!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : exprIsZeroLengthArraySection06 - variable
!*                               and/or expr Contain References to variable
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 18, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
!*                               with Deferred Length
!*  SECONDARY FUNCTIONS TESTED : and expr is a Zero Length Array Section of
!*                               variable
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is an allocated allocatable variable, it is deallocated if
!*  expr is an array of different shape or any of the corresponding length
!*  type parameter values of variable and expr differ. If variable is or
!*  becomes an unallocated allocatable variable, then it is allocated with
!*  each deferred type parameter equal to the corresponding type parameters
!*  of expr, with the shape of expr, and with each lower bound equal to the
!*  corresponding element of LBOUND(expr).
!*
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM exprIsZeroLengthArraySection06

    CHARACTER(3) :: charArr1( 10 ) =&
        (/ 'abc', 'DEF', 'ghi', 'JKL', 'mno',&
           'PQR', 'stu', 'VWX', 'YZ1', '234' /)

    CHARACTER(3) :: charArr3( 0:9,-1:8,10 )
    CHARACTER(:), ALLOCATABLE :: charArrAlloc( :,:,: )


    charArr3 =&
        RESHAPE((/ (((charArr1( k ), k = 1, 10), j = 1, 10), i = 1, 10) /),&
                                                                (/ 10,10,10 /))


    CALL Initialize( 10_4 )
    charArrAlloc = charArrAlloc( :,5:4,3:8 )
    CALL CheckIt((/ 10,1,10,3, 0,1,0,3, 6,1,6,3 /),&
                        'Zero Length Array Section (10x0x6):', 20_4)


    CALL Initialize( 30_4 )
    charArrAlloc = charArrAlloc( 1:0,-1:3,3:7 )( :1 )
    CALL CheckIt((/ 0,1,0,1, 5,1,5,1, 5,1,5,1 /),&
            'Zero Length Array Section -- with CHARACTER(1) (0x5x5):', 40_4)


    CALL Initialize( 50_4 )
    charArrAlloc = charArrAlloc( 3:5,8:7,2:1 ) //&
                    charArrAlloc( 8:6:-1,0:-1,4:3 )( 2: )
    CALL CheckIt((/ 3,1,3,5, 0,1,0,5, 0,1,0,5 /),&
            'Zero Length Array Section -- with CHARACTER(5) (3x0x0):', 60_4)


    CONTAINS

        SUBROUTINE Initialize( failRC )
            INTEGER(4) :: failRC


            charArrAlloc = charArr3
            CALL CheckIt((/ 10,0,9,3, 10,-1,8,3, 10,1,10,3 /),&
                            'Initialization (10x10x10):', failRC)

        END SUBROUTINE Initialize


        SUBROUTINE CheckIt(limits, title, failRC)
            INTEGER :: limits( 4,3 )
            CHARACTER(*) :: title
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( charArrAlloc ))    CALL zzrc( failRC )


            PRINT *
            PRINT *, title

            DO i = 1, 3
                PRINT 10, i, limits( :,i )
                PRINT 20, SIZE(charArrAlloc, i), LBOUND(charArrAlloc, i),&
                                UBOUND(charArrAlloc, i), LEN( charArrAlloc )

10              FORMAT(I1,')  Expected:',4(I3))
20              FORMAT('    Found:   ',4(I3))


                IF (SIZE(charArrAlloc, i) /= limits( 1,i ))&
                                        CALL zzrc( (failRC + 1_4) )
                IF (LBOUND(charArrAlloc, i) /= limits( 2,i ))&
                                        CALL zzrc( (failRC + 2_4) )
                IF (UBOUND(charArrAlloc, i) /= limits( 3,i ))&
                                        CALL zzrc( (failRC + 3_4) )
                IF (LEN( charArrAlloc ) /= limits( 4,i ))&
                                        CALL zzrc( (failRC + 4_4) )
            END DO

        END SUBROUTINE CheckIt

END PROGRAM exprIsZeroLengthArraySection06
