! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/typeConversion/exprIsIntrinsicFunc01.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*                               Conversion
!*
!*  DATE                       : November  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array
!*  SECONDARY FUNCTIONS TESTED : and expr is the Intrinsic Function RESHAPE()
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
!*  For a numeric intrinsic assignment statement, variable and expr may have
!*  different numeric types or different kind type parameters, in which case
!*  the value of expr is converted to the type and kind type parameter of
!*  variable according to the rules of Table 7.9.
!*
!*  Table 7.9: Numeric conversion and the assignment statement
!*  Type of variable Value Assigned
!*  integer     INT (expr, KIND = KIND (variable))
!*  real        REAL (expr, KIND = KIND (variable))
!*  complex     CMPLX (expr, KIND = KIND (variable))
!*  Note: The functions INT, REAL, CMPLX, and KIND are the generic functions
!*  defined in 13.7.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM exprIsIntrinsicFunc01

    TYPE :: tB(N1,K1)    ! (20,2)
        INTEGER, KIND            :: K1
        INTEGER, LEN             :: N1
        INTEGER(K1), ALLOCATABLE :: b( : )
    END TYPE tB

    TYPE, EXTENDS(tB) :: tD(K2)    ! (20,2,4)
        INTEGER, KIND            :: K2
        INTEGER(K2), ALLOCATABLE :: d
    END TYPE tD


    INTEGER(4) :: n

    REAL(16) :: real16Arr( 10 ) = [ (REAL(j, 16), j = 10, 1, - 1) ]

    TYPE(tD(:,2,4)), ALLOCATABLE :: d( :,: )


    d = RESHAPE([ (tD(20,2,4)(real16Arr,(i - 1)), i = 1, 10) ], [ 5,2 ])
    IF (.NOT. ALLOCATED( d )) CALL zzrc( 10_4 )


    PRINT *, SIZE(d, 1), SIZE(d, 2)
    IF (SIZE(d, 1) /= 5) CALL zzrc( 11_4 )
    IF (SIZE(d, 2) /= 2) CALL zzrc( 12_4 )


    DO i = 1, 2
        PRINT *

        DO j = 1, 5
            m = ((i - 1) * 5) + j
            n = INT(m, 4)

            IF (.NOT. ALLOCATED( d( j,i )%d )) CALL zzrc( (20_4 + n) )
            IF (.NOT. ALLOCATED( d( j,i )%d )) CALL zzrc( (30_4 + n) )


            PRINT *, j, i, SIZE( d( j,i )%b ),&
                     d( j,i )%d, '(', d( j,i )%b, ')'


            IF (SIZE( d( j,i )%b ) /= 10)   CALL zzrc( (40_4 + n) )
            IF (d( j,i )%d /= (m - 1))      CALL zzrc( (50_4 + n) )


            m = (m - 1) * 10

            DO k = 1, 10
                n = INT((m + k), 4)
                IF (d( j,i )%b( k ) /= (11 - k)) CALL zzrc( (100_4 + n) )
            END DO
        END DO
    END DO

END PROGRAM exprIsIntrinsicFunc01
