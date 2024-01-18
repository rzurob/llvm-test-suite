!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : April 20, 2013
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  based on F2003/dtparam/allocate/allocBasePoly03.f
!*
!*  Module subroutine inside a submodule can access a variable in the
!*  host module scope (in this case allocate a pointer to a derived type
!*  object defined in the host).
!*
!*  Verify that the results match the values of the original test case.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod

    IMPLICIT NONE

    TYPE base(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(l1) :: typeOfStuff
        INTEGER(k1), POINTER :: stuff( : )
    END TYPE base

    CLASS(base(8,5)), POINTER :: BasePtr
    INTERFACE
        module subroutine NewBasePtr( rc )
            INTEGER(4) :: rc
        END subroutine NewBasePtr
    END INTERFACE

END MODULE baseMod

SUBMODULE (baseMod) subMod

    CONTAINS

        module subroutine NewBasePtr( rc )
            INTEGER(4) :: rc


            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(BasePtr, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, "ALLOCATE(BasePtr(8,5),STAT=", stat, ") ", errmsg
                call zzrc( rc )
            END IF

        END subroutine NewBasePtr

END SUBMODULE subMod

PROGRAM allocBasePoly03
    USE baseMod

    IMPLICIT NONE

    IF ( ASSOCIATED( basePtr ) )                error stop  10_4


    call NewBasePtr( 20_4 )


    IF (.NOT. ASSOCIATED( basePtr ))            error stop  21_4

    IF (basePtr%k1 /= 8)                        error stop  22_4
    IF (KIND( basePtr%stuff ) /= 8)             error stop  23_4

    IF (basePtr%l1 /= 5)                        error stop  24_4
    IF (LEN( basePtr%typeOfStuff ) /= 5)        error stop  25_4


    DEALLOCATE( basePtr )

    IF ( ASSOCIATED( basePtr ) )                error stop  30_4

END PROGRAM allocBasePoly03
