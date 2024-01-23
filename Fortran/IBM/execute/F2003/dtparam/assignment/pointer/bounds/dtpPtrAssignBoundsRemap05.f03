!***********************************************************************
!* =====================================================================
!*
!*                               and Remapping)
!*
!*  DATE                       : March 13, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Remapping for a non-Polymorphic
!*                               instance of an Array of a Derived Type
!*                               Hierarchy to a non-Polymorphic 4-Dimension
!*                               Array
!*  SECONDARY FUNCTIONS TESTED : data-target is a Dynamically Allocated
!*                               Object and contains zero sized Elements
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

    TYPE tType(kT,lT1,lT2,lT3)
        INTEGER, KIND :: kT
        INTEGER, LEN :: lT1
        INTEGER, LEN :: lT2
        INTEGER, LEN :: lT3

        COMPLEX(kT) :: data( lT1,lT1:lT2,lT2:lT3 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Type16NotEqual

    END TYPE tType

    INTEGER :: low
    INTEGER :: high


    CONTAINS

        ELEMENTAL LOGICAL FUNCTION Type16NotEqual(this, o)
            CLASS(tType(16,*,*,*)), INTENT(in) :: this
            CLASS(tType(16,*,*,*)), INTENT(in) :: o


            Type16NotEqual = .TRUE.
            IF ((this%kT == o%kT) .AND.&
                (this%lT1 == o%lT1) .AND.&
                (this%lT2 == o%lT2) .AND.&
                (this%lT3 == o%lT3) .AND.&
                ( ALL(this%data == o%data) )) THEN
                Type16NotEqual = .FALSE.
            END IF

        END FUNCTION Type16NotEqual

END MODULE mType


MODULE mContainerType
    USE mType

    IMPLICIT NONE

    TYPE tContainerType(lC1,kC,lC2,lC3,lC4)
        INTEGER, LEN :: lC1
        INTEGER, KIND :: kC
        INTEGER, LEN :: lC2
        INTEGER, LEN :: lC3
        INTEGER, LEN :: lC4

        TYPE(tType(kC,lC2,lC3,lC4)) :: data( lC1 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => ContainerType16NotEqual

    END TYPE tContainerType

    CONTAINS

        LOGICAL FUNCTION ContainerType16NotEqual(this, o)
            CLASS(tContainerType(*,16,*,*,*)), INTENT(in) :: this
            CLASS(tContainerType(*,16,*,*,*)), INTENT(in) :: o


            ContainerType16NotEqual = .TRUE.
            IF ((this%lC1 == o%lC1) .AND.&
                (this%kC  ==  o%kC) .AND.&
                (this%lC2 == o%lC2) .AND.&
                (this%lC3 == o%lC3) .AND.&
                (this%lC4 == o%lC4) .AND.&
                (.NOT. ANY(this%data /= o%data))) THEN
                        ContainerType16NotEqual = .FALSE.
            END IF

            IF ( ContainerType16NotEqual )&
                PRINT *, "ContainerType16NotEqual() ==", ContainerType16NotEqual

        END FUNCTION ContainerType16NotEqual


        FUNCTION NewContainer(lC1, lC2, lC3, lC4, n)
            INTEGER :: lC1
            INTEGER :: lC2
            INTEGER :: lC3
            INTEGER :: lC4
            INTEGER :: n

            TYPE(tContainerType(lC1,16,lC2,lC3,lC4)),&
                                POINTER :: NewContainer( : )

            INTEGER :: stat
            CHARACTER(255) :: errMsg


            PRINT *, "NewContainer(",lC1,",",lC2,",",lC3,",",lC4,",",n,")"
            ALLOCATE(NewContainer( n ), STAT=stat, ERRMSG=errMsg)
            IF (stat /= 0) THEN
                PRINT *, "NewContainer(STAT=", stat, ") ", errMsg
                NewContainer => NULL( )
            END IF

        END FUNCTION NewContainer

END MODULE mContainerType


PROGRAM dtpPtrAssignBoundsRemap05
    USE mContainerType

    IMPLICIT NONE

    TYPE(tContainerType(:,16,:,:,:)), POINTER :: dataTarget( : )
    TYPE(tContainerType(:,16,:,:,:)), POINTER :: dataPointerObject( :,:,:,: )


    !
    !  3rd Dimension of container%data%data( ) is zero length
    !
    dataTarget => NewContainer(1, 2, 3, 2, 16)
    IF (.NOT. ASSOCIATED( dataTarget ))         ERROR STOP 5_4

    dataPointerObject( 1:2,1:2,1:2,1:2 ) => dataTarget
    low = SIZE( dataTarget )

    CALL Verify(RESHAPE([ 1, 2, 1, 2, 1, 2, 1, 2 ], [ 2,4 ]), 10_4)

    DEALLOCATE( dataTarget )


    !
    !  2nd Dimension of container%data%data( ) is zero length
    !
    dataTarget => NewContainer(1, 2, 1, 2, 71)
    IF (.NOT. ASSOCIATED( dataTarget ))         ERROR STOP 45_4

    dataPointerObject( 0:2,0:2,0:2,0:2 ) => dataTarget
    low = SIZE( dataTarget )

    CALL Verify(RESHAPE([ 0, 2, 0, 2, 0, 2, 0, 2 ], [ 2,4 ]), 50_4)

    DEALLOCATE( dataTarget )


    !
    !  1st Dimension of container%data%data( ) is zero length
    !
    dataTarget => NewContainer(1, (low - high), low, high, 256)
    IF (.NOT. ASSOCIATED( dataTarget ))         ERROR STOP 145_4

    dataPointerObject( -1:2,-1:2,-1:2,-1:2 ) => dataTarget
    low = SIZE( dataTarget )

    CALL Verify(RESHAPE([ -1, 2, -1, 2, -1, 2, -1, 2 ], [ 2,4 ]), 150_4)

    DEALLOCATE( dataTarget )


    !
    !  container%data( ) is zero length
    !
    dataTarget => NewContainer((low - high), low, high, (high + 1), 24)
    IF (.NOT. ASSOCIATED( dataTarget ))         ERROR STOP 445_4

    dataPointerObject( -1:2,0:2,1:2,2:2 ) => dataTarget

    CALL Verify(RESHAPE([ -1, 2, 0, 2, 1, 2, 2, 2 ], [ 2,4 ]), 450_4)

    DEALLOCATE( dataTarget )


    CONTAINS

        SUBROUTINE Verify(bounds, rc)
            INTEGER :: bounds( 2,4 )
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j
            INTEGER :: k
            INTEGER :: l

            INTEGER :: m


            IF ( ANY(SHAPE( dataPointerObject ) /=&
                    [   (bounds( 2,1 ) - bounds( 1,1 ) + 1),&
                        (bounds( 2,2 ) - bounds( 1,2 ) + 1),&
                        (bounds( 2,3 ) - bounds( 1,3 ) + 1),&
                        (bounds( 2,4 ) - bounds( 1,4 ) + 1) ]) ) CALL zzrc( rc )


            DO i = 1, 4
                IF (LBOUND(dataPointerObject, i) /= bounds( 1,i ))&
                                            CALL zzrc( (rc + INT(i, 4)) )
                IF (UBOUND(dataPointerObject, i) /= bounds( 2,i ))&
                                            CALL zzrc( (rc + INT(i, 4) + 5_4) )
            END DO


            m = 1
            DO l = bounds( 1,4 ), bounds( 2,4 )
                DO k = bounds( 1,3 ), bounds( 2,3 )
                    DO j = bounds( 1,2 ), bounds( 2,2 )
                        DO i = bounds( 1,1 ), bounds( 2,1 )
                            IF (dataPointerObject( i,j,k,l )&
                                          /= dataTarget( m ))&
                                            CALL zzrc( (rc + INT(m, 4) + 10_4) )
                            m = m + 1
                        END DO
                    END DO
                END DO
            END DO

            high = low + 1

        END SUBROUTINE Verify

END PROGRAM dtpPtrAssignBoundsRemap05
