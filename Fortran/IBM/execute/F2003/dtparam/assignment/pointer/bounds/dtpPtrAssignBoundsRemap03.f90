!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignBoundsRemap03
!*  TEST CASE TITLE            : POINTER Assignment (with Bounds Specification
!*                               and Remapping)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 12, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Remapping for a non-Polymorphic
!*                               instance of a Container Type Array to a
!*                               (non-Polymorphic) 7-Dimension Array
!*  SECONDARY FUNCTIONS TESTED : Where LBOUND() != 1 for some Dimensions
!*                               of data-pointer-object and data-target
!*                               is an Array Section
!*
!*  DRIVER STANZA              : xlf2003
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

    TYPE tType(kT,lT)
        INTEGER, KIND   :: kT
        INTEGER, LEN    :: lT

        INTEGER(kT)     :: id
        CHARACTER(lT)   :: lable

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Type2NotEqual

    END TYPE tType

    CONTAINS

        LOGICAL FUNCTION Type2NotEqual(this, o)
            CLASS(tType(2,*)), INTENT(in) :: this
            CLASS(tType(2,*)), INTENT(in) :: o


            Type2NotEqual = .FALSE.
            IF ((this%id /= o%id)   .OR.&
                (this%kT /= o%kT)   .OR.&
                (this%lT /= o%lT)   .OR.&
                (this%lable /= o%lable)) THEN
                Type2NotEqual = .TRUE.
            END IF

            IF ( Type2NotEqual ) PRINT *, "Type2NotEqual() ==", Type2NotEqual

        END FUNCTION Type2NotEqual

END MODULE mType


MODULE mContainer
    USE mType

    IMPLICIT NONE

    TYPE tContainer(lC1,kC2,lC2)
        INTEGER, LEN :: lC1
        INTEGER, KIND :: kC2
        INTEGER, LEN :: lC2

        CHARACTER(lC1) :: name
        TYPE(tType(kC2,lC2)), ALLOCATABLE :: data

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Container2NotEqual

    END TYPE tContainer


    TYPE(tContainer(:,2,:)), TARGET, ALLOCATABLE :: dataTarget( : )
    TYPE(tContainer(:,2,:)), POINTER :: dataPointerObject( :,:,:,:,:,:,: )


    CONTAINS

        LOGICAL FUNCTION Container2NotEqual(this, o)
            CLASS(tContainer(*,2,*)), INTENT(in) :: this
            CLASS(tContainer(*,2,*)), INTENT(in) :: o


            Container2NotEqual = .TRUE.
            IF ((o%lC1 == this%lC1)     .AND.&
                (o%kC2 == this%kC2)     .AND.&
                (o%lC2 == this%lC2)     .AND.&
                (o%name == this%name)) THEN
                IF ((.NOT. ALLOCATED( o%data )) .AND.&
                    (.NOT. ALLOCATED( this%data ))) THEN
                    Container2NotEqual = .FALSE.

                ELSE IF (( ALLOCATED( o%data ) ) .AND.&
                         ( ALLOCATED( this%data ) )) THEN
                    IF (.NOT. (o%data /= this%data)) THEN
                        Container2NotEqual = .FALSE.
                    END IF
                END IF
            END IF

            IF ( Container2NotEqual )&
                PRINT *, "Container2NotEqual() ==", Container2NotEqual

        END FUNCTION Container2NotEqual

END MODULE mContainer


PROGRAM dtpPtrAssignBoundsRemap03
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE TContainerInit( )
            USE mContainer
        END SUBROUTINE TContainerInit

        SUBROUTINE PointerAssociate( )
            USE mContainer
        END SUBROUTINE PointerAssociate

        SUBROUTINE VerifyPointer( )
            USE mContainer
        END SUBROUTINE VerifyPointer
    END INTERFACE


    CALL TContainerInit( )
    CALL PointerAssociate( )
    CALL VerifyPointer( )

END PROGRAM dtpPtrAssignBoundsRemap03


SUBROUTINE TContainerInit( )
    USE mContainer

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: stat
    CHARACTER(255) :: errMsg


    ALLOCATE(tContainer(4,2,3) :: dataTarget( 130 ), STAT=stat, ERRMSG=errMsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(STAT=", stat, ")", errMsg
        CALL zzrc( 10_4 )
    END IF

    DO i = 1, 130
        WRITE(dataTarget( i )%name, "('C',I3.3)") i
        IF (MOD(i, 5) /= 0) THEN
            dataTarget( i )%data = tType(2,3)(INT(i, 2),'T--')
            WRITE(dataTarget( i )%data%lable( 2: ), '(Z2.2)') i
        END IF
    END DO

END SUBROUTINE TContainerInit


SUBROUTINE PointerAssociate( )
    USE mContainer

    IMPLICIT NONE


    dataPointerObject( 1:2,0:1,1:2,0:1,1:2,0:1,1:2 ) => dataTarget( 2: )

END SUBROUTINE PointerAssociate


SUBROUTINE VerifyPointer( )
    USE mContainer

    IMPLICIT NONE

    INTEGER :: i = 1
    INTEGER :: m1, m2, m3, m4, m5, m6, m7


    DO m1 = 1, 7
        IF (MOD(m1, 2) == 0) THEN
            IF (LBOUND(dataPointerObject, m1) /= 0)&
                        CALL zzrc( (20_4 + INT(m1, 4)) )
            IF (UBOUND(dataPointerObject, m1) /= 1)&
                        CALL zzrc( (30_4 + INT(m1, 4)) )

        ELSE
            IF (LBOUND(dataPointerObject, m1) /= 1)&
                        CALL zzrc( (20_4 + INT(m1, 4)) )
            IF (UBOUND(dataPointerObject, m1) /= 2)&
                        CALL zzrc( (30_4 + INT(m1, 4)) )
        END IF
    END DO


    DO m7 = 1, 2
        DO m6 = 0, 1
            DO m5 = 1, 2
                DO m4 = 0, 1
                    DO m3 = 1, 2
                        DO m2 = 0, 1
                            DO m1 = 1, 2
                                i = i + 1
                                PRINT *, i, ") dataPointerObject(",&
                                            m1, ",", m2, ",", m3, ",",&
                                            m4, ",", m5, ",", m6, ",", m7, ")"
                                CALL Verify( )
                            END DO
                        END DO
                    END DO
                END DO
            END DO
        END DO
    END DO

    CONTAINS

        SUBROUTINE Verify( )

            IF (dataPointerObject( m1,m2,m3,m4,m5,m6,m7 )&
                    /= dataTarget( i ))     CALL zzrc( (100_4 + INT(i, 4)) )

        END SUBROUTINE Verify

END SUBROUTINE VerifyPointer
