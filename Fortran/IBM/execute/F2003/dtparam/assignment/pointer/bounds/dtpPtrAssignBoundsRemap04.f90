!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignBoundsRemap04
!*                               and Remapping)
!*
!*  DATE                       : March 13, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Remapping for an instance of an Array
!*                               of a Derived Type Hierarchy to a 3-Dimension
!*                               Array
!*  SECONDARY FUNCTIONS TESTED : Both data-pointer-object and data-target are
!*                               Unlimited Polymorphic Objects
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mType
    IMPLICIT NONE

    TYPE tType(kT)
        INTEGER, KIND :: kT

        INTEGER(kT) :: id

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Type8NotEqual

    END TYPE tType

    CONTAINS

        LOGICAL FUNCTION Type8NotEqual(this, o)
            CLASS(tType(8)), INTENT(in) :: this
            CLASS(tType(8)), INTENT(in) :: o


            Type8NotEqual = .TRUE.
            IF ((this%kT == o%kT) .AND.&
                (this%id == o%id)) THEN
                Type8NotEqual = .FALSE.
            END IF

            IF ( Type8NotEqual ) PRINT *, "Type8NotEqual() ==", Type8NotEqual

        END FUNCTION Type8NotEqual

END MODULE mType


MODULE mExtendedType
    USE mType

    IMPLICIT NONE

    TYPE, EXTENDS(tType) :: tExtendedType(lE)
        INTEGER, LEN :: lE

        REAL(kT) :: data( lE )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => ExtendedType8NotEqual

    END TYPE tExtendedType

    CONTAINS

        LOGICAL FUNCTION ExtendedType8NotEqual(this, o)
            CLASS(tExtendedType(8,*)), INTENT(in) :: this
            CLASS(tType(8)), INTENT(in) :: o


            ExtendedType8NotEqual = .TRUE.
            SELECT TYPE ( o )
                TYPE IS (tExtendedType(8,*))
                    IF (.NOT. (this%tType /= o%tType)) THEN
                        IF ((this%kT == o%kT)   .AND.&
                            (this%lE == o%lE)   .AND.&
                            ( ALL(this%data == o%data) )) THEN
                                ExtendedType8NotEqual = .FALSE.
                        END IF
                    END IF

                CLASS DEFAULT
                    PRINT *, "ExtendedType8NotEqual() Unknown Type"
            END SELECT

            IF ( ExtendedType8NotEqual )&
                PRINT *, "ExtendedType8NotEqual() ==", ExtendedType8NotEqual

        END FUNCTION ExtendedType8NotEqual

END MODULE mExtendedType


MODULE mData
    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 10
    INTEGER, PARAMETER :: M = N * N

    INTEGER, PARAMETER :: L = 5
    INTEGER, PARAMETER :: RL = L * M

END MODULE mData


PROGRAM dtpPtrAssignBoundsRemap04
    USE mExtendedType
    USE mData

    IMPLICIT NONE

    INTERFACE
        FUNCTION MangleArray(array, s)
            USE mExtendedType

            CLASS(*), TARGET :: array( : )
            INTEGER :: s

            CLASS(*), POINTER :: MangleArray( :,:,: )
        END FUNCTION MangleArray
    END INTERFACE

    INTEGER :: i
    INTEGER :: j

    INTEGER :: low
    INTEGER :: high

    INTEGER(4) :: rc = 10_4
    REAL(8) :: real8Array( RL ) = [ ((1.0_8 / REAL(i, 8)), i = 1, RL) ]

    TYPE(tExtendedType(8,L)), TARGET :: dataTarget( N,N )
    TYPE(tExtendedType(8,:)), POINTER :: dataPointerObject( :,:,: )


    DO j = 1, N
        DO i = 1, N
            low = ((j - 1) * N) + i
            low = ((low - 1) * L) + 1
            high = low + L - 1

            dataTarget( i,j ) = tExtendedType(8,L)(i,real8Array( low:high ))
        END DO
    END DO


    high = -1
    DO j = 1, N
        i = MOD(j, 3) + 1
        high = MOD((high + 1), 3)

        PRINT *, "dataTarget(", i, ":,", j, ")", high

        SELECT TYPE (dt => MangleArray(dataTarget( i:,j ), high))
            TYPE IS (tExtendedType(8,*))
                dataPointerObject => dt

            CLASS DEFAULT
                CALL zzrc( rc )
        END SELECT

        CALL CheckPointer(i, j, high, rc)

        rc = rc + 20_4
    END DO

    CONTAINS

        SUBROUTINE CheckPointer(i, j, s, rc)
            INTEGER :: i
            INTEGER :: j
            INTEGER :: s
            INTEGER(4) :: rc

            INTEGER :: k
            INTEGER :: l
            INTEGER :: m
            INTEGER :: n


            DO m = 1, 3
                IF (SIZE(dataPointerObject, m) /= s)&
                                CALL zzrc( (rc + INT(m, 4)) )
            END DO


            n = i
            DO m = 1, s
                DO l = 1, s
                    DO k = 1, s
                        IF (dataPointerObject( k,l,m ) /= dataTarget( n,j ))&
                                            CALL zzrc( (rc + 10_4 + INT(n, 4)) )
                        n = n + 1
                    END DO
                END DO
            END DO

        END SUBROUTINE CheckPointer

END PROGRAM dtpPtrAssignBoundsRemap04


FUNCTION MangleArray(array, s)
    USE mExtendedType

    CLASS(*), TARGET :: array( : )
    INTEGER :: s

    CLASS(*), POINTER :: MangleArray( :,:,: )


    MangleArray( 1:s,1:s,1:s ) => array

END FUNCTION MangleArray
