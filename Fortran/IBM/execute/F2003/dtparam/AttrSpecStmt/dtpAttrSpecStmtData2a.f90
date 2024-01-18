!***********************************************************************
!* =====================================================================
!*
!*                               (based on dtpAttrSpecStmtData2)
!*
!*  DATE                       : April 22, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  -- DATA statement
!*  A variable that appears in a DATA statement and has not been typed
!*  previously may appear in a subsequent type declaration only if that
!*  declaration confirms the implicit typing.
!*
!*  Using non-Polymorphic Types
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE m

    TYPE :: dt0(k0,l0)
        INTEGER, KIND :: k0 = 1
        INTEGER, LEN  :: l0 = 1

        CONTAINS

            PROCEDURE :: ModFun0

    END TYPE

    TYPE, EXTENDS(dt0) :: dt1(k1,l1)
        INTEGER(k0), KIND :: k1 = 1
        INTEGER(k0), LEN  :: l1 = 1

        INTEGER(k1)       :: r( l1 )    ! = k1

        CONTAINS

            PROCEDURE :: ModFun1

    END TYPE

    TYPE, EXTENDS(dt1) :: dt2(k2,l2)
        INTEGER(k1), KIND :: k2 = 1
        INTEGER(k1), LEN  :: l2 = 1

        INTEGER(k2)       :: i( l2 )    ! = k2
        CHARACTER(l2)     :: c( l2 )    ! = CHAR( k2 )
        TYPE(dt2(k0,l0,k1,l0,k2,l2)), POINTER :: ptr    ! => NULL()

        CONTAINS

            PROCEDURE :: ModFun2

    END TYPE

    INTEGER, PARAMETER  :: N = 2044

    CONTAINS

        FUNCTION ModFun0( arg )
            CLASS(dt0(1,*)), INTENT(in) :: arg

            TYPE(dt0(1,arg%L0)) :: ModFun0


            ModFun0 = arg

        END FUNCTION

        FUNCTION ModFun1( arg )
            CLASS(dt1(1,*,4,*)), INTENT(in) :: arg

            COMPLEX ::  ModFun1( 2 )


            ModFun1( 1 ) = (arg%k0,arg%l0)
            ModFun1( 2 ) = (arg%k1,arg%l1)

        END FUNCTION

        FUNCTION ModFun2( arg )
            CLASS(dt2(1,*,4,*,8,*)), INTENT(in) :: arg

            TYPE(dt2(1,arg%l0,4,arg%l1,8,arg%l2)) ModFun2


            IF (SIZE( ModFun2%i ) .NE. arg%l2)  STOP 22
            ModFun2%i = -Arg%i

        END FUNCTION

END MODULE


PROGRAM dtpAttrSpecStmtData2a
    USE m

    IMPLICIT    TYPE(dt0(1,3))          (t),&
                TYPE(dt1(1,3,4,5))      (u),&
                TYPE(dt2(1,3,4,5,8,7))  (v)


    DATA t0 / 1 * dt0(1,3)() /,&
         u1 / dt1(1,3,4,5)(dt0=dt0(1,3)(),r=[ 1, 2, 3, 4, 5 ]) /

    DATA v2 / dt2(1,3,4,5,8,7)(                             &
                      dt1=dt1(1,3,4,5)([ 1, 2, 3, 4, 5 ]),  &
                      i=[ 1, 2, 3, 4, 5, 6, 7 ],            &
                      c=CHAR([ 1, 2, 3, 4, 5, 6, 7 ]),      &
                      ptr=NULL( ) ) /


    TYPE(dt0(1,3))          :: t0
    TYPE(dt0(1,3))          :: t01

    TYPE(dt1(1,3,4,5))      :: u1

    TYPE(dt2(1,3,4,5,8,7))  :: v2
!   TYPE(dt2(1,3,4,5,8,7))  :: v22  ! ... This Declaration should be Implicit


    t01 = t0%ModFun0( )

    IF ( ANY(u1%r .NE. [ 1, 2, 3, 4, 5 ]) )                 STOP 10
    IF ( ANY(u1%ModFun1( ) .NE. [ (1,3), (4,5) ]) )         STOP 11

    IF ( ANY(v2%i .NE. [ 1, 2, 3, 4, 5, 6, 7 ]) )           STOP 12
    IF ( ANY(v2%c .NE. CHAR( [ 1, 2, 3, 4, 5, 6, 7 ] )) )   STOP 13
    IF ( ANY(v2%dt1%ModFun1( ) .NE. [ (1,3), (4,5) ]) )     STOP 14

    v22 = v2%ModFun2( )
    IF ( ANY(v22%i .NE. [ -1, -2, -3, -4, -5, -6, -7 ]) )   STOP 15

END PROGRAM dtpAttrSpecStmtData2a
