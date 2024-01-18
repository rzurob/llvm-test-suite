!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! DATE                       : 08/05/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component
! DESCRIPTION                : Use of  procedure declaration statement & procedure component with NoPass and PASS attr. and DEFFERED attr.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE M

    TYPE, EXTENDS(OBJECT) :: T
     INTEGER :: N
    END TYPE

    TYPE, ABSTRACT :: OBJECT (k1,LEN1)
       INTEGER, KIND :: k1
       INTEGER, LEN :: LEN1
       REAL(kind=k1), DIMENSION(1) :: SIDES1=(/0.0/)
       REAL(kind=k1), DIMENSION(2) :: SIDES2=(/0.0,0.0/)
       REAL(kind=k1), DIMENSION(3) :: SIDES3=(/0.0,0.0,0.0/)
       CHARACTER(LEN=LEN1)      :: NAME='Object'
   END TYPE OBJECT

END MODULE


PROGRAM procdeclfn03
USE M
END PROGRAM procdeclfn03

