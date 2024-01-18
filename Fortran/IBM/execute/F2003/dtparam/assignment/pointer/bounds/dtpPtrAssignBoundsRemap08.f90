!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignBoundsRemap08
!*  TEST CASE TITLE            : POINTER Assignment (with Bounds Specification
!*                               and Remapping)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Remapping for an Array instance
!*                               of a Container Object to a Rank 3 Array
!*                               data-pointer-object
!*  SECONDARY FUNCTIONS TESTED : Where one Dimension of data-pointer-object
!*                               is of zero length
!*
!*  DRIVER STANZA              : xlf2003
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
            PROCEDURE, PASS :: NotEqual => BaseNotEqual

    END TYPE tBase

    CONTAINS

        LOGICAL FUNCTION BaseNotEqual(this, that)
            CLASS(tBase(2,*)), INTENT(in) :: this
            CLASS(tBase(2,*)), INTENT(in) :: that


            BaseNotEqual = .FALSE.
            IF ((this%k1 /= that%k1)   .OR.&
                (this%l1 /= that%l1)   .OR.&
                ( ANY(this%id /= that%id) )) THEN
                BaseNotEqual = .TRUE.
            END IF

        END FUNCTION BaseNotEqual

END MODULE mBase


MODULE mContainer
    USE mBase

    TYPE :: tContainer(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        TYPE(tBase(k1,l1)) :: data

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => ContainerNotEqual

    END TYPE tContainer

    CONTAINS

        LOGICAL FUNCTION ContainerNotEqual(this, that)
            CLASS(tContainer(2,*)), INTENT(in) :: this
            CLASS(tContainer(2,*)), INTENT(in) :: that


            ContainerNotEqual = .FALSE.
            IF ((this%k1 /= that%k1)    .OR.&
                (this%l1 /= that%l1)    .OR.&
                (this%data /= that%data)) THEN
                ContainerNotEqual = .TRUE.
            END IF

        END FUNCTION ContainerNotEqual

END MODULE mContainer


PROGRAM dtpPtrAssignBoundsRemap08
    USE mContainer

    IMPLICIT NONE

    INTERFACE
        FUNCTION Remap(original, l1, bounds, n)
            USE mContainer

            TYPE(tContainer(2,l1)), TARGET :: original( : )
            INTEGER :: l1
            INTEGER :: bounds( :,: )
            INTEGER :: n

            TYPE(tContainer(2,l1)), POINTER :: Remap( :,:,: )
        END FUNCTION Remap

        SUBROUTINE CheckRemap(remap, original, l1, bounds, n, rc)
            USE mContainer

            TYPE(tContainer(2,:)), POINTER :: remap( :,:,: )
            TYPE(tContainer(2,l1)), TARGET :: original( : )
            INTEGER :: l1
            INTEGER :: bounds( :,: )
            INTEGER :: n
            INTEGER(4) :: rc
        END SUBROUTINE CheckRemap
    END INTERFACE


    INTEGER, PARAMETER :: N = 25

    INTEGER :: i
    INTEGER :: j

    INTEGER :: bounds( 2,3 )

    INTEGER(4) :: rc = 10_4

    TYPE(tContainer(2,1)), TARGET :: dataTarget( N )
    TYPE(tContainer(2,:)), POINTER :: dataPointerObject( :,:,: )


    DO i = 1, N
        dataTarget( i )%data%id = [ INT(i, 2) ]
    END DO


    DO i = 1, N
        SELECT CASE (MOD(i, 3))
            CASE (0)
                bounds =&
                    RESHAPE([ i, (i - 1), i, (i + 1), i, (i + 1) ], [ 2,3 ])

            CASE (1)
                bounds =&
                    RESHAPE([ i, (i + 1), i, (i - 1), i, (i + 1) ], [ 2,3 ])

            CASE (2)
                bounds =&
                    RESHAPE([ i, (i + 1), i, (i + 1), i, (i - 1) ], [ 2,3 ])
        END SELECT


        dataPointerObject => Remap(dataTarget, 1, bounds, i)

        PRINT *, "main()		", bounds, "	",&
                     (LBOUND(dataPointerObject, j),&
                      UBOUND(dataPointerObject, j), j = 1, 3)


        CALL CheckRemap(dataPointerObject, dataTarget, 1, bounds, i, rc)

        rc = rc + 10_4
    END DO


END PROGRAM dtpPtrAssignBoundsRemap08


FUNCTION Remap(original, l1, bounds, n)
    USE mContainer

    IMPLICIT NONE

    TYPE(tContainer(2,l1)), TARGET :: original( : )
    INTEGER :: l1
    INTEGER :: bounds( :,: )
    INTEGER :: n

    TYPE(tContainer(2,l1)), POINTER :: Remap( :,:,: )

    INTEGER :: i


    Remap(  bounds( 1,1 ):bounds( 2,1 ),&
            bounds( 1,2 ):bounds( 2,2 ),&
            bounds( 1,3 ):bounds( 2,3 )     ) => original( n: )

    PRINT *
    PRINT *, "Remap()	", bounds, "	",&
                (LBOUND(Remap, i), UBOUND(Remap, i), i = 1, 3)

END FUNCTION Remap


SUBROUTINE CheckRemap(remap, original, l1, bounds, n, rc)
    USE mContainer

    IMPLICIT NONE

    TYPE(tContainer(2,:)), POINTER :: remap( :,:,: )
    TYPE(tContainer(2,l1)), TARGET :: original( : )
    INTEGER :: l1
    INTEGER :: bounds( :,: )
    INTEGER :: n
    INTEGER(4) :: rc

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: l
    INTEGER :: m

    INTEGER :: expected( 2 )


    IF (.NOT. ASSOCIATED( remap ))  CALL zzrc( rc )
    IF (remap%l1 /= l1)             CALL zzrc( (rc + 1_4) )
    IF (SIZE( remap ) /= 0)         CALL zzrc( (rc + 2_4) )


    PRINT *, "CheckRemap()	", bounds, "	",&
                (LBOUND(remap, i), UBOUND(remap, i), i = 1, 3)

    l = MOD(n, 3) + 1
    DO i = 1, 3
        !
        !  As per the Fortran Standard, LBOUND()/UBOUND() do something
        !  different for a zero-length Array Dimension.  Thus, adjust
        !  the expected results if we're checking the Dimension exepcted
        !  to be zero-length.
        !
        IF (i == l) THEN
            expected = [ 1, 0 ]
        ELSE
            expected = [ bounds( 1,i ), bounds( 2,i ) ]
        END IF

        IF (LBOUND(remap, i) /= expected( 1 ))&
                CALL zzrc( (rc + INT((i * 2), 4) + 1_4) )
        IF (UBOUND(remap, i) /= expected( 2 ))&
                CALL zzrc( (rc + INT((i * 2), 4) + 2_4) )
    END DO


    m = n
    DO k = LBOUND(remap, 3), UBOUND(remap, 3)
        DO j = LBOUND(remap, 2), UBOUND(remap, 2)
            DO i = LBOUND(remap, 1), UBOUND(remap, 1)
                IF (remap( i,j,k ) /= original( m )) THEN
                    PRINT *, "original(",m,")%data%id =",original( m )%data%id
                    PRINT *, "remap(", i, ",", j, ",", k,&
                            ")%data%id =", remap( i,j,k )%data%id
                    CALL zzrc( (rc + 9_4) )
                END IF

                m = m + 1
            END DO
        END DO
    END DO


END SUBROUTINE CheckRemap
