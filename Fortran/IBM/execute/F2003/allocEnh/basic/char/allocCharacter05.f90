!*  ===================================================================
!*
!*                               Intrinsic Type
!*
!*  DATE                       : September  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is an Array of Type CHARACTER with a
!*                               both a different Shape and Length Type
!*                               Parameter Value
!*
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

PROGRAM allocCharacter05

    INTEGER(4) :: i

    CHARACTER(10) :: cALong( 10 ) = (/ ('IBMibmTest', i = 1, 10) /)
    CHARACTER(4) :: cAShort( 4 ) = (/ ('tEsT', i = 1, 4) /)

    CHARACTER(:), ALLOCATABLE :: cAA( : )


    ALLOCATE( CHARACTER(6) :: cAA( 6 ) )

    cAA = (/ ('fortran', i = 1, 6) /)


    IF (.NOT. ALLOCATED( cAA )) CALL zzrc( 10_4 )
    IF (SIZE( cAA ) /= 6)       CALL zzrc( 20_4 )


    cAA = cALong


    IF (.NOT. ALLOCATED( cAA )) CALL zzrc( 30_4 )
    IF (SIZE( cAA ) /= 10)      CALL zzrc( 40_4 )


    DO i = 1_4, 10_4
        PRINT *, "cAA(", i, ") = '", cAA( i ), "' (", cALong( i ), ")"

        IF (LEN( cAA( i ) ) /= 10)      CALL zzrc( (50_4 + i) )
        IF (cAA( i ) /= cALong( i ))   CALL zzrc( (60_4 + i) )
    END DO


    cAA = cAShort


    IF (.NOT. ALLOCATED( cAA )) CALL zzrc( 70_4 )
    IF (SIZE( cAA ) /= 4)       CALL zzrc( 80_4 )


    DO i = 1_4, 4_4
        PRINT *, "cAA(", i, ") = '", cAA( i ), "' (", cAShort( i ), ")"

        IF (LEN( cAA( i ) ) /= LEN( cAShort( i )))  CALL zzrc( (90_4 + i) )
        IF (cAA( i ) /= 'tEsT')                     CALL zzrc( (100_4 + i) )
    END DO

END PROGRAM allocCharacter05
