MODULE mod

    IMPLICIT NONE

    TYPE t(k,l1,l2)
        INTEGER, KIND :: k
        INTEGER(k), LEN :: l1
        INTEGER(k), LEN :: l2

        REAL(k) :: a( l1:l2,l2,(l2 - l1) )
    END TYPE t

END MODULE mod


MODULE cMod
    USE mod

    IMPLICIT NONE

    TYPE c(k,l1,l2)
        INTEGER, KIND :: k
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2

        TYPE(t(k,l1,(l2 - l1))) :: content

        CONTAINS

            PROCEDURE, NOPASS :: NewC

    END TYPE c

    CONTAINS

        SUBROUTINE NewC(this, l1, l2, rc)
            CLASS(c(8,l1,l2)), ALLOCATABLE :: this
            INTEGER :: l1
            INTEGER :: l2

            INTEGER(4) :: rc

            INTEGER :: stat

            CHARACTER(255) :: errmsg


            ALLOCATE(this, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *,"ALLOCATE(this(8,",l1,",",l2,"),STAT=",stat,') ',errmsg
                CALL zzrc( rc )
            END IF

        END SUBROUTINE NewC

END MODULE cMod
