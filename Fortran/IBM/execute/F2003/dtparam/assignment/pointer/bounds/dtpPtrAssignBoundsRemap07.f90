!***********************************************************************
!* =====================================================================
!*
!*                               and Remapping)
!*
!*  DATE                       : March 23, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Remapping for an Array instance
!*                               of a Base Object to a Rank 3, 2, 1 Array
!*  SECONDARY FUNCTIONS TESTED : Where the Remapping is performed Recursively
!*                               using one Dimension of the Remapped Array
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mBase
    IMPLICIT NONE

    TYPE tBase(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1) :: id( l1 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => BaseNotEqual

    END TYPE tBase

    CONTAINS

        LOGICAL FUNCTION BaseNotEqual(this, o)
            CLASS(tBase(4,*)), INTENT(in) :: this
            CLASS(tBase(4,*)), INTENT(in) :: o


            BaseNotEqual = .FALSE.
            IF ((this%k1 /= o%k1)   .OR.&
                (this%l1 /= o%l1)   .OR.&
                ( ANY(this%id /= o%id) )) THEN
                BaseNotEqual = .TRUE.
            END IF

        END FUNCTION BaseNotEqual

END MODULE mBase


MODULE mExt1
    USE mBase

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tExt1
        TYPE(tBase(k1,l1)), POINTER :: ext( : )

        CONTAINS

            PROCEDURE, PASS :: Check => CheckExt1

    END TYPE tExt1

    CONTAINS

        SUBROUTINE CheckExt1(this, o, rc)
            CLASS(tExt1(4,*)), INTENT(in) :: this
            CLASS(tBase(4,*)), INTENT(in) :: o( : )
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: n


            IF ( ANY(SHAPE( this%ext ) /= [ 1 ]) )  CALL zzrc( rc )

            IF (LBOUND(this%ext, 1) /= 0)           CALL zzrc( (rc + 1_4) )
            IF (UBOUND(this%ext, 1) /= 0)           CALL zzrc( (rc + 6_4) )


            n = 0
            DO i = LBOUND(this%ext, 1), UBOUND(this%ext, 1)
                n = n + 1
                IF (this%ext( i ) /= o( n )) THEN
                    PRINT *, "CheckExt1() this%ext(", i, ") = ",&
                                                    this%ext( i )%id
                    PRINT *, "CheckExt1() o(", n, ") = ", o( n )%id
                    CALL zzrc( (rc + 10_4 + INT(n, 4)) )
                END IF
            END DO

        END SUBROUTINE CheckExt1

END MODULE mExt1


MODULE mExt2
    USE mBase

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tExt2
        TYPE(tBase(k1,l1)), POINTER :: ext( :,: )

        CONTAINS

            PROCEDURE, PASS :: Check => CheckExt2

    END TYPE tExt2

    CONTAINS

        SUBROUTINE CheckExt2(this, o, rc)
            CLASS(tExt2(4,*)), INTENT(in) :: this
            CLASS(tBase(4,*)), INTENT(in) :: o( : )
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j
            INTEGER :: n


            IF ( ANY(SHAPE( this%ext ) /= [ 2, 1 ]) )   CALL zzrc( rc )

            IF (LBOUND(this%ext, 1) /= -1)              CALL zzrc( (rc + 1_4) )
            IF (LBOUND(this%ext, 2) /= -1)              CALL zzrc( (rc + 2_4) )

            IF (UBOUND(this%ext, 1) /= 0)               CALL zzrc( (rc + 6_4) )
            IF (UBOUND(this%ext, 2) /= -1)              CALL zzrc( (rc + 7_4) )


            n = 0
            DO j = LBOUND(this%ext, 2), UBOUND(this%ext, 2)
                DO i = LBOUND(this%ext, 1), UBOUND(this%ext, 1)
                    n = n + 1
                    IF (this%ext( i,j ) /= o( n )) THEN
                        PRINT *, "CheckExt2() this%ext(", i,&
                                 ",", j, ") = ", this%ext( i,j )%id
                        PRINT *, "CheckExt2() o(", n, ") = ", o( n )%id
                        CALL zzrc( (rc + 10_4 + INT(n, 4)) )
                    END IF
                END DO
            END DO

        END SUBROUTINE CheckExt2

END MODULE mExt2


MODULE mExt3
    USE mBase

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tExt3
        TYPE(tBase(k1,l1)), POINTER :: ext( :,:,: )

        CONTAINS

            PROCEDURE, PASS :: Check => CheckExt3

    END TYPE tExt3

    CONTAINS

        SUBROUTINE CheckExt3(this, o, rc)
            CLASS(tExt3(4,*)), INTENT(in) :: this
            CLASS(tBase(4,*)), INTENT(in) :: o( : )
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j
            INTEGER :: k
            INTEGER :: n


            IF ( ANY(SHAPE( this%ext ) /= [ 3, 3, 3 ]) )CALL zzrc( rc )

            IF (LBOUND(this%ext, 1) /= 2)               CALL zzrc( (rc + 1_4) )
            IF (LBOUND(this%ext, 2) /= 4)               CALL zzrc( (rc + 2_4) )
            IF (LBOUND(this%ext, 3) /= 6)               CALL zzrc( (rc + 3_4) )

            IF (UBOUND(this%ext, 1) /= 4)               CALL zzrc( (rc + 6_4) )
            IF (UBOUND(this%ext, 2) /= 6)               CALL zzrc( (rc + 7_4) )
            IF (UBOUND(this%ext, 3) /= 8)               CALL zzrc( (rc + 8_4) )


            n = 0
            DO k = LBOUND(this%ext, 3), UBOUND(this%ext, 3)
                DO j = LBOUND(this%ext, 2), UBOUND(this%ext, 2)
                    DO i = LBOUND(this%ext, 1), UBOUND(this%ext, 1)
                        n = n + 1
                        IF (this%ext( i,j,k ) /= o( n )) THEN
                            PRINT *, "CheckExt3() this%ext(", i, ",",&
                                     j, ",", k, ") = ", this%ext( i,j,k )%id
                            PRINT *, "CheckExt3() o(", n, ") = ", o( n )%id
                            CALL zzrc( (rc + 10_4 + INT(n, 4)) )
                        END IF
                    END DO
                END DO
            END DO

        END SUBROUTINE CheckExt3

END MODULE mExt3


PROGRAM dtpPtrAssignBoundsRemap07
    USE mExt1
    USE mExt2
    USE mExt3

    IMPLICIT NONE


    INTEGER :: i

    TYPE(tBase(4,1)), TARGET :: basePtr( 27 )


    DO i = 1, 27
        basePtr( i )%id = [ i ]
    END DO

    CALL Diverge(basePtr, 3, 50_4)


    CONTAINS

        RECURSIVE SUBROUTINE Diverge(pBase, n, rc)
            CLASS(tBase(4,*)), TARGET :: pBase( : )
            INTEGER :: n
            INTEGER(4) :: rc

            TYPE(tExt1(4,1)) :: pExt1
            TYPE(tExt2(4,1)) :: pExt2
            TYPE(tExt3(4,1)) :: pExt3


            SELECT CASE ( n )
                CASE (3)
                    pExt3%ext( 2:4,4:6,6:8 ) => pBase
                    CALL pExt3%Check(pBase, rc)
                    CALL Diverge(pExt3%ext( :,4,6 ), (n - 1), (rc + 50_4))

                CASE (2)
                    pExt2%ext( -1:0,-1:-1 ) => pBase
                    CALL pExt2%Check(pBase, rc)
                    CALL Diverge(pExt2%ext( :,-1 ), (n - 1), (rc + 50_4))

                CASE (1)
                    pExt1%ext( 0:0 ) => pBase
                    CALL pExt1%Check(pBase, rc)
                    CALL Diverge(pExt1%ext, (n - 1), (rc + 50_4))
            END SELECT

        END SUBROUTINE Diverge

END PROGRAM dtpPtrAssignBoundsRemap07
