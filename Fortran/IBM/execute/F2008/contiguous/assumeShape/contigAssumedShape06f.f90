! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Copy-in/out for assumed shape arrays
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*    - Assumed-shape dummy argument is of derived/polymorphic type
!*    - Dummy argument has contiguous attribute
!*    - Actual argument non-contiguous
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE Mod
      IMPLICIT NONE

      TYPE :: DT0
        CHARACTER(10) :: C0 = "XL Fortran"
        INTEGER       :: I0(10) = -99
      END TYPE

      TYPE, EXTENDS(DT0)  :: DT1
        INTEGER       :: I1(6) = -66
        CHARACTER(12) :: C1 = "IBM compiler"
      END TYPE

      CONTAINS

      SUBROUTINE Sub(Arg0, Arg1)
        INTEGER :: I, J
        TYPE(DT0), CONTIGUOUS :: Arg0(:)
        CLASS(DT0), CONTIGUOUS :: Arg1(:)

        IF ( .NOT. IS_CONTIGUOUS(Arg0) ) ERROR STOP 10
        IF ( .NOT. IS_CONTIGUOUS(Arg1) ) ERROR STOP 11

        DO I = 1, SIZE(Arg0)
            IF ( ANY(Arg0(I)%I0 .NE. [((2*I-1)*J, J=1,10)]) ) ERROR STOP 12
            IF ( Arg0(I)%C0     .NE.           "XL Fortran" ) ERROR STOP 13
        END DO

        DO I = 1, SIZE(Arg1)
            SELECT TYPE ( Arg1 )
                 CLASSIS (DT0)
                   IF ( ANY(Arg1(I)%I0 .NE. [((3*I-2)*J, J=1,10)]) ) ERROR STOP 14
                   IF ( Arg1(I)%C0     .NE.    "XL Fortran" ) ERROR STOP 15

                 CLASSIS (DT1)
                   IF ( ANY(Arg1(I)%I0 .NE. [((3*I-2)*J, J=1,10)]) ) ERROR STOP 16
                   IF ( Arg1(I)%C0     .NE.           "XL Fortran" ) ERROR STOP 17
                   IF ( ANY(Arg1(I)%I1 .NE.                   -66) ) ERROR STOP 18
                   IF ( Arg1(I)%C1     .NE.         "IBM compiler" ) ERROR STOP 19

                 CLASSDEFAULT
                     STOP 23

            END SELECT
        END DO

        CALL InnerSub(Arg0, Arg1)
      END SUBROUTINE Sub

      SUBROUTINE InnerSub(Arg0, Arg1)
        INTEGER :: I, J
        TYPE(DT0) :: Arg0(5)
        CLASS(DT0) :: Arg1(4)

        DO I = 1, SIZE(Arg0)
            IF ( ANY(Arg0(I)%I0 .NE.  [((2*I-1)*J, J=1,10)]) ) ERROR STOP 20
            IF ( Arg0(I)%C0     .NE.            "XL Fortran" ) ERROR STOP 21
        END DO

        DO I = 1, SIZE(Arg1)
            IF ( ANY(Arg1(I)%I0 .NE.  [((3*I-2)*J, J=1,10)]) ) ERROR STOP 22
            IF ( Arg1(I)%C0     .NE.            "XL Fortran" ) ERROR STOP 23
        END DO
      END SUBROUTINE InnerSub
END MODULE
PROGRAM contigAssumedShape06f
      USE Mod
      IMPLICIT NONE

      INTEGER :: I, J
      TYPE(DT0) :: T0(10)
      CLASS(DT0), POINTER :: T1(:)
      CLASS(DT0), ALLOCATABLE :: T2(:)

      IF ( .NOT. IS_CONTIGUOUS(T0) )     ERROR STOP 101

      ALLOCATE( T1(10), SOURCE = DT1() )
      IF ( .NOT. IS_CONTIGUOUS(T1) )     ERROR STOP 102

      ALLOCATE( DT0 :: T2(10) )
      IF ( .NOT. IS_CONTIGUOUS(T2) )     ERROR STOP 103

      DO I = 1, 10
        T0(I)%I0 = [(I*J, J=1,10)]
        T1(I)%I0 = T0(I)%I0
        T2(I)%I0 = T1(I)%I0
      END DO

      CALL Sub(T0(1:10:2),T0(1:10:3))
      CALL Sub(T0(1:10:2),T1(1:10:3))
      CALL Sub(T0(1:10:2),T2(1:10:3))

END PROGRAM contigAssumedShape06f
