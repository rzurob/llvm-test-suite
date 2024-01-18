!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocatedAllocatable01 - Basic Tests:
!*                               Non-CHARACTER Intrinsic Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : July 21, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar/Array of
!*                               Intrinsic Type
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar/Array of Intrinsic Type
!*                               with the same Shape and Length Type Parameter
!*                               Values
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
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
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM allocatedAllocatable01

    INTEGER(4) :: i

    REAL(8), ALLOCATABLE :: scalarReal8Alloc
    COMPLEX(16), ALLOCATABLE, DIMENSION( : ) :: arrayComplex16Alloc

    CHARACTER(256) :: errMsg


100 FORMAT("Scalar (",A6,") = '",F5.3,"'")

    ALLOCATE(scalarReal8Alloc, SOURCE=1.234_8, STAT=iStat, ERRMSG=errMsg)

    IF (iStat /= 0) THEN
        WRITE(0, *) "ALLOCATE(scalarReal8Alloc) <", iStat, "> ", errMsg
        CALL zzrc( 10_4 )

    ELSE IF (scalarReal8Alloc /= 1.234_8) THEN
        WRITE(0, 100) 'ALLOCATE()', scalarReal8Alloc
        WRITE(0, *) "scalarReal8Alloc != 1.234_8"

        CALL zzrc( 20_4 )
    END IF

    WRITE(0, 100) 'Before', scalarReal8Alloc


    scalarReal8Alloc = 4.567_8

    IF (scalarReal8Alloc /= 4.567_8) THEN
        WRITE(0, 100) 'Wrong', scalarReal8Alloc
        WRITE(0, *) "scalarReal8Alloc != 4.567_8"

        CALL zzrc( 30_4 )
    END IF

    WRITE(0, 100) 'After', scalarReal8Alloc


    WRITE(0, *)


200 FORMAT("(",F5.3,",",F5.3,")")

    ALLOCATE(arrayComplex16Alloc( 10 ), STAT=iStat,&
            SOURCE=(/ (CMPLX(i, i, 16), i = 1, 10) /), ERRMSG=errMsg)

    IF (iStat /= 0) THEN
        WRITE(0, *) "ALLOCATE(arrayComplex16Alloc( 10 )) <",&
                                                    iStat, "> ", errMsg
        CALL zzrc( 40_4 )
    END IF

    DO i = 1, 10
        IF (arrayComplex16Alloc( i ) /= CMPLX(i, i, 16)) THEN
            WRITE(0, 200) arrayComplex16Alloc( i )
            WRITE(0, *) "arrayComplex16Alloc(", i, ") != ", CMPLX(i, i, 16)

            CALL zzrc( (50_4 + i) )
        END IF
    END DO

    WRITE(0, *) "arrayComplex16Alloc (Before):"
    WRITE(0, 200) arrayComplex16Alloc


    arrayComplex16Alloc =&
        (/ (CMPLX((scalarReal8Alloc * i), (scalarReal8Alloc / i)), i = 1, 10) /)

    DO i = 1, 10
        IF (arrayComplex16Alloc( i ) /=&
            CMPLX((scalarReal8Alloc * i), (scalarReal8Alloc / i))) THEN

            WRITE(0, *) "arrayComplex16Alloc (Error):"
            WRITE(0, 200) arrayComplex16Alloc( i )
            WRITE(0, *) "arrayComplex16Alloc(", i, ") != ",&
                        CMPLX((scalarReal8Alloc * i), (scalarReal8Alloc / i))

            CALL zzrc( (60_4 + i) )
        END IF
    END DO

END PROGRAM allocatedAllocatable01
