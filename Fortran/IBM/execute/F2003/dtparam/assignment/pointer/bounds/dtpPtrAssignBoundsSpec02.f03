!***********************************************************************
!* =====================================================================
!*
!*                               and Remapping)
!*
!*  DATE                       : March 27, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Specification with an Unlimited
!*                               Polymorphic data-pointer-object
!*  SECONDARY FUNCTIONS TESTED : Where data-target is also an Unlimited
!*                               Polymorphic with a dynamic type of a
!*                               Base/Extended Derived Type
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
            PROCEDURE, PASS :: NotEqual => NotEqualBase

    END TYPE tBase

    CONTAINS

        LOGICAL FUNCTION NotEqualBase(this, that)
            CLASS(tBase(2,*)), INTENT(in) :: this
            CLASS(tBase(2,*)), INTENT(in) :: that


            NotEqualBase = .FALSE.
            IF ((this%k1 /= that%k1)   .OR.&
                (this%l1 /= that%l1)   .OR.&
                ( ANY(this%id /= that%id) )) THEN
                NotEqualBase = .TRUE.
            END IF

        END FUNCTION NotEqualBase

END MODULE mBase


MODULE m1Ext
    USE mBase

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: t1Ext(l2)
        INTEGER, LEN :: l2

        CHARACTER(l2) :: name

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => NotEqual1Ext

    END TYPE t1Ext

    CONTAINS

        LOGICAL FUNCTION NotEqual1Ext(this, that)
            CLASS(t1Ext(2,*,*)), INTENT(in) :: this
            CLASS(tBase(2,*)), INTENT(in) :: that


            NotEqual1Ext = .TRUE.
            SELECT TYPE( that )
                TYPE IS (t1Ext(2,*,*))
                    IF ((this%k1 == that%k1)        .AND.&
                        (this%l1 == that%l1)        .AND.&
                        (this%l2 == that%l2)        .AND.&
                        (this%name == that%name)    .AND.&
                        (.NOT. (this%tBase /= that%tBase))) THEN
                        NotEqual1Ext = .FALSE.
                    END IF

                CLASS DEFAULT
                    PRINT *, 'NotEqual1Ext():  Unknown Type'
            END SELECT

        END FUNCTION NotEqual1Ext

END MODULE m1Ext


MODULE m2Ext
    USE m1Ext

    IMPLICIT NONE

    TYPE, EXTENDS(t1Ext) :: t2Ext(k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN :: l3

        COMPLEX(k3) :: data( l3 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => NotEqual2Ext

    END TYPE t2Ext

    CONTAINS

        LOGICAL FUNCTION NotEqual2Ext(this, that)
            CLASS(t2Ext(2,*,*,8,*)), INTENT(in) :: this
            CLASS(tBase(2,*)), INTENT(in) :: that


            NotEqual2Ext = .TRUE.
            SELECT TYPE( that )
                TYPE IS (t2Ext(2,*,*,8,*))
                    IF ((this%k1 == that%k1)                        .AND.&
                        (this%k3 == that%k3)                        .AND.&
                        (this%l1 == that%l1)                        .AND.&
                        (this%l2 == that%l2)                        .AND.&
                        (this%l3 == that%l3)                        .AND.&
                        (SIZE( this%data ) == SIZE( that%data ))    .AND.&
                        ( ALL(this%data == that%data) )             .AND.&
                        (.NOT. (this%t1Ext /= that%t1Ext))) THEN
                        NotEqual2Ext = .FALSE.
                    END IF

                CLASS DEFAULT
                    PRINT *, 'NotEqual2Ext():  Unknown Type'
            END SELECT

        END FUNCTION NotEqual2Ext

END MODULE m2Ext


PROGRAM dtpPtrAssignBoundsSpec02
    USE m2Ext

    IMPLICIT NONE

    INTERFACE
        FUNCTION PtrAssign(this, bounds)
            IMPLICIT NONE

            CLASS(*), TARGET :: this( :,:,: )
            INTEGER :: bounds( : )

            CLASS(*), POINTER :: PtrAssign( :,:,: )
        END FUNCTION PtrAssign
    END INTERFACE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: l

    REAL(8), PARAMETER :: real8Array( 6 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 6) ]
    COMPLEX(8) :: cmplx8Array( 3 ) =&
        [ (CMPLX(real8Array( i ), real8Array( (i + 1) ),8), i = 1, 6, 2) ]

    INTEGER :: bounds( 3 ) = 0

    TYPE(tBase(2,1)), TARGET :: base( 3,3,3 )
    TYPE(t1Ext(2,1,2)), TARGET :: ext1( 2,2,2 )
    TYPE(t2Ext(2,1,2,8,3)), TARGET :: ext2( 1,1,1 )

    CLASS(*), POINTER :: ptr( :,:,: )


    l = 0
    DO k = 1, 3
        DO j = 1, 3
            DO i = 1, 3
                l = l + 1
                base( i,j,k ) = tBase(2,1)(INT(l, 2))

                IF ((i < 3)  .AND.  (j < 3)  .AND.  (k < 3)) THEN
                    ext1( i,j,k )%tBase = base( i,j,k )
                    WRITE(ext1( i,j,k )%name, '(I2.2)') l

                    IF ((i < 2)  .AND.  (j < 2)  .AND.  (k < 2)) THEN
                        ext2( i,j,k )%t1Ext = ext1( i,j,k )
                        ext2( i,j,k )%data = cmplx8Array
                    END IF
                END IF
            END DO
        END DO
    END DO


    ptr => PtrAssign(base, bounds)
    SELECT TYPE (ptr)
        TYPE IS (tBase(2,*))
            CALL Verify(base, bounds, 10_4)

        CLASS DEFAULT
            ERROR STOP 10_4
    END SELECT


    bounds = -1
    ptr => PtrAssign(ext1, bounds)
    SELECT TYPE (ptr)
        TYPE IS (t1Ext(2,*,*))
            CALL Verify(ext1, bounds, 50_4)

        CLASS DEFAULT
            ERROR STOP 50_4
    END SELECT


    bounds = 1
    ptr => PtrAssign(ext2, bounds)
    SELECT TYPE (ptr)
        TYPE IS (t2Ext(2,*,*,8,*))
            CALL Verify(ext2, bounds, 80_4)

        CLASS DEFAULT
            ERROR STOP 80_4
    END SELECT


    bounds = [ 1, 0, -1 ]
    ptr => PtrAssign(ptr, bounds)
    SELECT TYPE (ptr)
        TYPE IS (t2Ext(2,*,*,8,*))
            CALL Verify(ext2, bounds, 100_4)

        CLASS DEFAULT
            ERROR STOP 100_4
    END SELECT

    CONTAINS

        SUBROUTINE Verify(target, bounds, rc)
            USE m2Ext

            IMPLICIT NONE

            CLASS(tBase(2,*)) :: target( :,:,: )
            INTEGER :: bounds( : )
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j
            INTEGER :: k

            INTEGER :: l

            INTEGER :: m
            INTEGER :: n
            INTEGER :: o


            IF ( ANY(SHAPE( ptr ) /= SHAPE( target )) ) CALL zzrc( (rc + 2_4) )


            DO i = 1, 3
                IF (LBOUND(ptr, i) /= bounds( i ))&
                                        CALL zzrc( (rc + 2_4 + INT(i, 4)) )
                IF (UBOUND(ptr, i) /= (bounds( i ) + SIZE(ptr, i) - 1))&
                                        CALL zzrc( (rc + 6_4 + INT(i, 4)) )
            END DO


            l = 0
            DO k = LBOUND(ptr, 3), UBOUND(ptr, 3)
                o = 1 - bounds( 3 )

                DO j = LBOUND(ptr, 2), UBOUND(ptr, 2)
                    n = 1 - bounds( 2 )

                    DO i = LBOUND(ptr, 1), UBOUND(ptr, 1)
                        m = 1 - bounds( 1 )

                        !
                        !  This SELECT TYPE statement is required as Compiler
                        !  can't determine Dynamic Type at Compile time.  At
                        !  Run-Time, the SELECT TYPE statements above will
                        !  resolve the ambiguity.
                        !
                        SELECT TYPE (ptr)
                            CLASS IS (tBase(2,*))
                                IF (target( (i + m),(j + n),(k + o) )&
                                                        /= ptr( i,j,k ))&
                                        CALL zzrc( (rc + 10_4 + INT(l, 4)) )

                            CLASS DEFAULT
                                CALL zzrc( (rc + 1_4) )
                        END SELECT

                        l = l + 1
                    END DO
                END DO
            END DO

        END SUBROUTINE Verify

END PROGRAM dtpPtrAssignBoundsSpec02


FUNCTION PtrAssign(this, bounds)
    IMPLICIT NONE

    CLASS(*), TARGET :: this( :,:,: )
    INTEGER :: bounds( : )

    CLASS(*), POINTER :: PtrAssign( :,:,: )

    PtrAssign => NULL( )
    IF (SIZE( bounds ) == SIZE( SHAPE( this ) )) THEN
        PtrAssign( bounds( 1 ):,bounds( 2 ):,bounds( 3 ): ) => this
    END IF

END FUNCTION PtrAssign
