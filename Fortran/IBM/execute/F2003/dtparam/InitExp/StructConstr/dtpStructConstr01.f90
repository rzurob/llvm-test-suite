!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpStructConstr01
!*
!*  DATE                       : May 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Structure Constructor
!*  SECONDARY FUNCTIONS TESTED : With a zero Length/Sized Array Component
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mType
    IMPLICIT NONE

    TYPE tType(lowLen,highLen)
        INTEGER, LEN :: lowLen
        INTEGER, LEN :: highLen

        INTEGER :: array( lowLen:highLen )
    END TYPE tType


    CONTAINS

        SUBROUTINE CheckTType(type, lowLen, highLen, arraySize, rc)
            TYPE(tType(*,*)) :: type( : )
            INTEGER :: lowLen
            INTEGER :: highLen
            INTEGER :: arraySize
            INTEGER(4) :: rc

            INTEGER :: i

            PRINT *, "CheckTType(", lowLen, ",", highLen,&
                                    ",", arraySize, ",", rc, ")"
            PRINT *, SIZE( type ), type%lowLen, type%highLen

            IF (SIZE( type ) /= arraySize)      CALL zzrc( rc )
            IF (type( 1 )%lowLen /= lowLen)     CALL zzrc( (rc + 1_4) )
            IF (type( 1 )%highLen /= highLen)   CALL zzrc( (rc + 2_4) )

            DO i = 1, arraySize
                IF (SIZE( type( i )%array ) /= 0)&
                                    CALL zzrc( (rc + 10_4 + INT(i, 4)) )
            END DO

        END SUBROUTINE CheckTType

END MODULE mType


MODULE mContainer
    USE mType

    IMPLICIT NONE

    TYPE tContainer(containerKind,lowLen,highLen,extent1,extent2)
        INTEGER, KIND :: containerKind
        INTEGER, LEN :: lowLen
        INTEGER, LEN :: highLen
        INTEGER, LEN :: extent1
        INTEGER, LEN :: extent2

        TYPE(tType(lowLen,highLen)) :: typeArray( extent1,extent2 )
    END TYPE tContainer

    CONTAINS

        SUBROUTINE CheckTContainer(container, lowLen, highLen,&
                        extent1, extent2, shapeContainer, shapeTypeArray, rc)
            TYPE(tContainer(3,:,:,:,:)), POINTER :: container( :,: )
            INTEGER :: lowLen
            INTEGER :: highLen
            INTEGER :: extent1
            INTEGER :: extent2
            INTEGER :: shapeContainer( : )
            INTEGER :: shapeTypeArray( : )
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j
            INTEGER :: k
            INTEGER :: l
            INTEGER :: m


            IF (.NOT. ASSOCIATED( container ))  CALL zzrc( rc )

            IF ( ANY(SHAPE( container ) /= shapeContainer) )&
                                                CALL zzrc( (rc + 1_4) )

            IF (container%containerKind /= 3)   CALL zzrc( (rc + 2_4) )
            IF (container%lowLen /= lowLen)     CALL zzrc( (rc + 3_4) )
            IF (container%highLen /= highLen)   CALL zzrc( (rc + 4_4) )
            IF (container%extent1 /= extent1)   CALL zzrc( (rc + 5_4) )
            IF (container%extent2 /= extent2)   CALL zzrc( (rc + 6_4) )

            m = 0
            DO j = 1, SIZE(container, 2)
                DO i = 1, SIZE(container, 1)
                    DO l = 1, SIZE(container( i,j )%typeArray, 2)
                        DO k = 1, SIZE(container( i,j )%typeArray, 1)
                            CALL CheckTType(&
                                 container( i,j )%typeArray( k:k,l ), lowLen,&
                                    highLen, 1, (rc + 10_4 + INT((m * 20), 4)))
                            m = m + 1
                        END DO
                    END DO
                END DO
            END DO

        END SUBROUTINE CheckTContainer

END MODULE mContainer


PROGRAM dtpStructConstr01
    USE mContainer
    USE mType

    IMPLICIT NONE


    INTEGER :: array( 2 ) = [ 2, 1 ]

    TYPE(tType(:,:)), ALLOCATABLE, TARGET :: type( : )
    TYPE(tContainer(3,:,:,:,:)), POINTER :: container( :,: )


    ALLOCATE(tType(5,4) :: type( 12 ))
    type = tType(5,4)(array( 2:1 ))
    CALL CheckTType(type, 5, 4, 12, 10_4)


    ALLOCATE(tContainer(3,5,4,4,3) :: container( 2,2 ))
    container = tContainer(3,5,4,4,3)(RESHAPE(type, [ 4, 3 ]))
    CALL CheckTContainer(container, 5, 4, 4, 3, [ 2, 2 ], [ 4, 3 ], 20_4)

END PROGRAM dtpStructConstr01
