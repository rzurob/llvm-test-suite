!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : December  5, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Derived Type Container
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains an Array
!*                               Variable
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Where allocation-list (without type-spec:: and using:  a Container
!*  Derived Type) is:
!*  o  Arrays of Basic Derived Type
!*
!*  Types of allocation-list Objects:
!*  o  Variable
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod

    IMPLICIT NONE

    TYPE base(k1,k2,l1)
        INTEGER, KIND :: k1
        INTEGER, KIND :: k2
        INTEGER, LEN :: l1

        INTEGER(k1) :: i( l1 )
        COMPLEX(k2) :: a( k2,(2 * l1) )
    END TYPE base

END MODULE baseMod


MODULE containerMod
    USE baseMod

    IMPLICIT NONE

    TYPE container(k1,l,k2,n)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l
        INTEGER, KIND :: k2
        INTEGER, LEN :: n

        INTEGER(k1) :: i( (n * l) )
        TYPE(base(k1,k2,(l + n))), POINTER :: c( : )
    END TYPE container

    TYPE(container(8,10,16,5)), ALLOCATABLE :: cA( : )

    CONTAINS

        INTEGER FUNCTION AllocateContainer(n, m)
            INTEGER :: n
            INTEGER :: m

            INTEGER :: i
            INTEGER :: stat
            CHARACTER(255) :: errmsg


            ALLOCATE(cA( n ), STAT=AllocateContainer, ERRMSG=errmsg)
            IF (AllocateContainer /= 0) THEN
                PRINT *, 'ALLOCATE(cA(', n, '), STAT=',&
                            AllocateContainer, ') ', errmsg

            ELSE
                i = 0
                DO WHILE ((i < n)  .AND.  (AllocateContainer == 0))
                    i = i + 1
                    ALLOCATE(cA( i )%c( m ),&
                                STAT=AllocateContainer, ERRMSG=errmsg)
                    IF (AllocateContainer /= 0) THEN
                        PRINT *, 'ALLOCATE(cA(', i, ')%c(', n, '), STAT=',&
                                    AllocateContainer, ') ', errmsg
                    END IF
                END DO
            END IF

        END FUNCTION AllocateContainer

END MODULE containerMod


PROGRAM allocContainerVariable02
    USE containerMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: stat

    stat = AllocateContainer(3, 5)
    IF (stat /= 0)                      ERROR STOP 10_4


    IF (cA%l /= 10)                     ERROR STOP 20_4
    IF (cA%n /= 5)                      ERROR STOP 21_4
    IF ( ANY(SHAPE( cA ) /= [ 3 ]) )    ERROR STOP 22_4

    DO i = 1, 3
        IF ( ANY(SHAPE( cA( i )%i ) /= [ 50 ]) )&
                                        CALL zzrc( INT((i * 50), 4) )
        IF ( ANY(SHAPE( cA( i )%c ) /= [ 5 ]) )&
                                        CALL zzrc( INT(((i * 50) + 1), 4) )


        IF (cA( i )%c%l1 /= 15)         CALL zzrc( INT(((i * 50) + 2), 4) )

        DO j = 1, 5
            IF ( ANY(SHAPE( cA( i )%c( j )%i ) /= [ 15 ]) )&
                                        CALL zzrc( INT(((i * 50) + 10 + j), 4) )
            IF ( ANY(SHAPE( cA( i )%c( j )%a ) /= [ 16,30 ]) )&
                                        CALL zzrc( INT(((i * 50) + 20 + j), 4) )
        END DO
    END DO

END PROGRAM allocContainerVariable02
