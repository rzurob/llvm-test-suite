!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignBoundsSpec01
!*                               and Remapping)
!*
!*  DATE                       : March 26, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Specification with a Polymorphic
!*                               Base Derived Type data-pointer-object
!*  SECONDARY FUNCTIONS TESTED : Where data-target is also a Polymorphic
!*                               Base Derived Type with a dynamic type
!*                               of the Base/Extended Derived Type
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

            PROCEDURE, NOPASS :: PtrAssign => PtrAssignBase
            PROCEDURE, NOPASS :: Verify => VerifyBase

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

        FUNCTION PtrAssignBase(this, bounds)
            CLASS(tBase(2,*)), TARGET :: this( :,:,: )
            INTEGER :: bounds( : )

            CLASS(tBase(2,:)), POINTER :: PtrAssignBase( :,:,: )

            PtrAssignBase => NULL( )
            IF (SIZE( bounds ) == SIZE( SHAPE( this ) )) THEN
                PtrAssignBase( bounds( 1 ):,bounds( 2 ):,bounds( 3 ): ) => this
            END IF

        END FUNCTION PtrAssignBase

        SUBROUTINE VerifyBase(target, pointer, bounds, rc)
            CLASS(tBase(2,*)), TARGET :: target( :,:,: )
            CLASS(tBase(2,:)), POINTER :: pointer( :,:,: )
            INTEGER :: bounds( : )
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j
            INTEGER :: k

            INTEGER :: l

            INTEGER :: m
            INTEGER :: n
            INTEGER :: o


            IF (.NOT. ASSOCIATED( pointer ))            CALL zzrc( rc )
            IF (.NOT. ASSOCIATED(pointer, target))      CALL zzrc( (rc + 1_4) )

            IF ( ANY(SHAPE( pointer ) /= SHAPE( target )) )&
                                                        CALL zzrc( (rc + 2_4) )

            DO i = 1, 3
                IF (LBOUND(pointer, i) /= bounds( i ))&
                                        CALL zzrc( (rc + 2_4 + INT(i, 4)) )
                IF (UBOUND(pointer, i) /=&
                    (bounds( i ) + SIZE(pointer, i) - 1))&
                                        CALL zzrc( (rc + 6_4 + INT(i, 4)) )
            END DO

            l = 0
            DO k = LBOUND(pointer, 3), UBOUND(pointer, 3)
                o = 1 - bounds( 3 )

                DO j = LBOUND(pointer, 2), UBOUND(pointer, 2)
                    n = 1 - bounds( 2 )

                    DO i = LBOUND(pointer, 1), UBOUND(pointer, 1)
                        m = 1 - bounds( 1 )
                        IF (target( (i + m),(j + n),(k + o) ) /=&
                                                    pointer( i,j,k ))&
                                        CALL zzrc( (rc + 10_4 + INT(l, 4)) )

                        l = l + 1
                    END DO
                END DO
            END DO

        END SUBROUTINE VerifyBase

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


PROGRAM dtpPtrAssignBoundsSpec01
    USE m2Ext

    IMPLICIT NONE


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

    CLASS(tBase(2,:)), POINTER :: pBase( :,:,: )


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


    pBase => pBase%PtrAssign(base, bounds)
    CALL pBase%Verify(base, pBase, bounds, 10_4)


    bounds = -1
    pBase => pBase%PtrAssign(ext1, bounds)
    CALL pBase%Verify(ext1, pBase, bounds, 50_4)

    SELECT TYPE (pBase)
        TYPE IS (t1Ext(2,*,*))

        CLASS DEFAULT
            CALL zzrc( 200_4 )
    END SELECT


    bounds = 1
    pBase => pBase%PtrAssign(ext2, bounds)
    CALL pBase%Verify(ext2, pBase, bounds, 80_4)

    SELECT TYPE (pBase)
        TYPE IS (t2Ext(2,*,*,8,*))

        CLASS DEFAULT
            CALL zzrc( 201_4 )
    END SELECT


    bounds = [ 1, 0, -1 ]
    pBase => pBase%PtrAssign(pBase, bounds)
    CALL pBase%Verify(ext2, pBase, bounds, 100_4)

    SELECT TYPE (pBase)
        TYPE IS (t2Ext(2,*,*,8,*))

        CLASS DEFAULT
            CALL zzrc( 202_4 )
    END SELECT

END PROGRAM dtpPtrAssignBoundsSpec01
