!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignBoundsSpec04
!*  TEST CASE TITLE            : POINTER Assignment (with Bounds Specification
!*                               and Remapping)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : June  8, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Specification for an array of Derived
!*                               Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Where data-target is data-pointer-object
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

    TYPE tBase(len)
        INTEGER, LEN :: len

        CHARACTER(len) :: title
    END TYPE tBase

    TYPE(tBase(:)), POINTER :: base( : )

    CONTAINS

        SUBROUTINE CheckBase(lowerBound, upperBound, length, rc)
            INTEGER :: lowerBound
            INTEGER :: upperBound
            INTEGER :: length
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j


            j = upperBound - lowerBound + 1

            PRINT *, rc, lowerBOUND, upperBOUND, j, length,&
                     LBOUND(base, 1), UBOUND(base, 1), SIZE( base ), base%len

            IF (LBOUND(base, 1) /= lowerBound)      CALL zzrc( rc )
            IF (UBOUND(base, 1) /= upperBound)      CALL zzrc( (rc + 1_4) )
            IF (SIZE( base ) /= j)                  CALL zzrc( (rc + 2_4) )
            IF (base%len /= length)                 CALL zzrc( (rc + 3_4) )

            DO i = lowerBound, upperBound
                j = i - lowerBound + 48
                IF (LEN( base( i )%title ) /= length) THEN
                    CALL zzrc( (rc + 9_4 + INT(i, 4)) )

                ELSE IF (ICHAR( base( i )%title ) /= j) THEN
                    CALL zzrc( (rc + 19_4 + INT(i, 4)) )
                END IF
            END DO

        END SUBROUTINE CheckBase


        FUNCTION ReBound(cBase, bounds)
            CLASS(tBase(*)), TARGET :: cBase( :,:,: )
            INTEGER :: bounds( : )

            CLASS(tBase(:)), POINTER :: ReBound( :,:,: )


            ReBound( bounds( 1 ):,bounds( 2 ):,bounds( 3 ): ) => cBase

        END FUNCTION ReBound

END MODULE mBase


MODULE mExt
    USE mBase
    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tExt(kind2,len2)
        INTEGER, KIND :: kind2
        INTEGER, LEN :: len2

        REAL(kind2) :: array( (len2 * 2) )

        CONTAINS

            GENERIC :: OPERATOR(/=) => ExtNotEqual
            PROCEDURE, PASS :: ExtNotEqual => ExtNotEqual16

    END TYPE tExt

    CONTAINS

        SUBROUTINE TExtInit( ext )
            TYPE(tExt(*,16,*)) :: ext( :,:,: )

            INTEGER :: i
            INTEGER :: j
            INTEGER :: k
            INTEGER :: m
            INTEGER :: arraySize

            REAL(16), ALLOCATABLE :: real16Array( : )
            CHARACTER(:), ALLOCATABLE :: nameArray( : )


            arraySize = SIZE( ext ) * (ext%len2 * 2)
            real16Array = [ ((1.0_16 / REAL(i, 16)), i = 1, arraySize) ]


            ALLOCATE(CHARACTER(ext%len) :: nameArray( SIZE( ext ) ))
            DO i = 1, SIZE( ext )
                WRITE(nameArray( i ), '(I1,A1)')&
                        ((i - 1) / 26), CHAR( (65 + MOD((i - 1), 26)) )
            END DO


            m = 1
            DO k = 1, UBOUND(ext, 3)
                DO j = 1, UBOUND(ext, 2)
                    DO i = 1, UBOUND(ext, 1)
                        ext( i,j,k ) =&
                            tExt(ext%len,16,ext%len2)(&
                                            nameArray( ((m / 4) + 1) ),&
                                            real16Array( m:(m + 3) ))
                        m = m + 4
                    END DO
                END DO
            END DO


            DEALLOCATE( nameArray )
            DEALLOCATE( real16Array )

        END SUBROUTINE TExtInit

        LOGICAL FUNCTION ExtNotEqual16(this, that)
            CLASS(tExt(*,16,*)), INTENT(in) :: this
            CLASS(tExt(*,16,*)), INTENT(in) :: that

            ExtNotEqual16 = .TRUE.
            IF ((this%len == that%len)                      .AND.&
                (LEN( this%title ) == LEN( that%title ))    .AND.&
                (this%title == that%title)                  .AND.&
                (this%len2 == that%len2)                    .AND.&
                (SIZE( this%array ) == (this%len2 * 2))     .AND.&
                (SIZE( this%array ) == SIZE( that%array ))  .AND.&
                ( ALL(this%array == that%array) )) THEN
                ExtNotEqual16 = .FALSE.
            END IF

        END FUNCTION ExtNotEqual16

END MODULE mExt


PROGRAM dtpPtrAssignBoundsSpec04
    USE mExt

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE ReSpecifyBaseBounds( lowerBound )
            INTEGER :: lowerBound
        END SUBROUTINE ReSpecifyBaseBounds
    END INTERFACE

    INTEGER :: i
    INTEGER :: bounds( 3 )
    INTEGER(4) :: rc = 10

    TYPE(tExt(2,16,2)), TARGET :: ext( 3,3,3 )
    CLASS(tBase(:)), POINTER :: pCBase( :,:,: )


    ALLOCATE(base( 10 ), SOURCE=[ (tBase(1)(CHAR( i )), i = 48, 57) ])

    DO i = -50, 50, 10
        CALL ReSpecifyBaseBounds( i )
        CALL CheckBase(i, (i + 9), 1, rc)
        rc = rc + 30_4
    END DO


    rc = 400_4
    bounds = [ -1 , 0 , 1 ]

    pCBase => ext
    CALL TExtInit( ext )

    DO i = 1, 3
        PRINT *
        PRINT *, 'rc =', rc, 'bounds = [', bounds, ']'

        pCBase => ReBound(pCBase, bounds)
        CALL CheckBounds(ext, bounds, rc)

        rc = rc + 100_4
        bounds = CSHIFT(bounds, 1)
    END DO


    CONTAINS

        SUBROUTINE CheckBounds(ext, bounds, rc)
            TYPE(tExt(*,16,*)) :: ext( :,:,: )
            INTEGER :: bounds( : )
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j
            INTEGER :: k

            INTEGER :: l
            INTEGER :: m
            INTEGER :: n

            INTEGER :: p


            IF (SIZE( pCBase ) /= SIZE( ext ))   CALL zzrc( rc )


            DO i = 1, 3
                IF (SIZE(pCBase, i) /= SIZE(ext, i))&
                                        CALL zzrc( (rc + 10_4 + INT(i, 4)) )
                IF (LBOUND(pCBase, i) /= bounds( i ))&
                                        CALL zzrc( (rc + 13_4 + INT(i, 4)) )
                IF (UBOUND(pCBase, i) /= (bounds( i ) + SIZE(ext, i) - 1))&
                                        CALL zzrc( (rc + 16_4 + INT(i, 4)) )
            END DO


            SELECT TYPE (pCBase)
                TYPE IS (tBase(*))
                    CALL zzrc( (rc + 1_4) )

                TYPE IS (tExt(*,16,*))
                    IF (pCBase%len2 /= ext%len2) CALL zzrc( (rc + 20_4) )

                    p = 1
                    n = 0
                    DO k = bounds( 3 ), (bounds( 3 ) + SIZE(pCBase, 3) - 1)
                        m = 0
                        n = n + 1
                        DO j = bounds( 2 ), (bounds( 2 ) + SIZE(pCBase, 2) - 1)
                            l = 0
                            m = m + 1
                            DO i = bounds( 1 ), (bounds( 1 )+SIZE(pCBase, 1)-1)
                                l = l + 1
                                PRINT *, '(', i, j, k, ')  (', l, m, n, ')', p
                                IF (pCBase( i,j,k ) /= ext( l,m,n ))&
                                            CALL zzrc( (rc + 20_4 + INT(p, 4)) )
                                p = p + 1
                            END DO
                        END DO
                    END DO

                CLASS DEFAULT
                    CALL zzrc( (rc + 2_4) )
            END SELECT

        END SUBROUTINE CheckBounds

END PROGRAM dtpPtrAssignBoundsSpec04


SUBROUTINE ReSpecifyBaseBounds( lowerBound )
    USE mExt

    IMPLICIT NONE

    INTEGER :: lowerBound


    base( lowerBound: ) => base

END SUBROUTINE ReSpecifyBaseBounds
