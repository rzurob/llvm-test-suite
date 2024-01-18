!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : aceNestedActualArg01
!*                               with DTP
!*
!*  DATE                       : November 21, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Nested Array
!*                               Constructors for a Derived Type (with
!*                               Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : and is the Acutal Argument to a User
!*                               Defined FUNCTION
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  1)  Testing the format of the Array Constructor:
!*      * Brackets:  []
!*      * Where the ac-value-list contains:
!*            o Nested Array Constructors
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As the Actual Argument in a User Defined SUBROUTINE/FUNCTION
!*        call,
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod

    IMPLICIT NONE

    TYPE base(k1,l1,l2)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2

        CHARACTER(l1) :: lable
        REAL(k1) :: array( l2 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual4
            PROCEDURE, PASS :: NotEqual4 => Base4NotEqual

    END TYPE base


    TYPE(base(4,:,:)), POINTER :: baseA( : )

    CHARACTER(3) :: strList( 9 ) =&
        [ 'IBM', 'XLF', 'TST', 'DTP', 'F03', 'ace', 'ICE', 'DEV', '131' ]


    CONTAINS

        LOGICAL FUNCTION Base4NotEqual(this, o)
            CLASS(base(4,*,*)), INTENT(in) :: this
            CLASS(base(4,*,*)), INTENT(in) :: o


            Base4NotEqual = .TRUE.
            IF ((this%k1 == o%k1) .AND.&
                (this%l1 == o%l1) .AND.&
                (this%l2 == o%l2) .AND.&
                (this%lable == o%lable) .AND.&
                ( ALL(this%array == o%array) )) THEN
                Base4NotEqual = .FALSE.
            END IF

        END FUNCTION Base4NotEqual

END MODULE bMod


PROGRAM aceNestedActualArg01
    USE bMod

    IMPLICIT NONE

    INTERFACE
        INTEGER(4) FUNCTION VerifyActualArg(array, rc)
            USE bMod

            IMPLICIT NONE

            TYPE(base(4,*,*)) :: array( : )
            INTEGER(4) :: rc
        END FUNCTION VerifyActualArg
    END INTERFACE


    INTEGER :: i
    INTEGER :: j
    INTEGER :: stat
    INTEGER(4) :: rc

    CHARACTER(255) :: errmsg

    REAL(4) :: rA( 27 ) = [ ((1.0_4 / REAL(i, 4)), i = 1, 27) ]

    TYPE(base(4,:,:)), POINTER :: item1
    TYPE(base(4,:,:)), POINTER :: item2
    TYPE(base(4,:,:)), POINTER :: item3

    TYPE(base(4,3,3)) :: item7
    TYPE(base(4,3,3)) :: item8
    TYPE(base(4,3,3)) :: item9

    TYPE(base(4,3,3)), POINTER :: itemPtr


    ALLOCATE(item1, SOURCE=base(4,3,3)(strList( 1 ),rA( 1:3 )))
    ALLOCATE(item2, SOURCE=base(4,3,3)(strList( 2 ),rA( 4:6 )))
    ALLOCATE(item3, SOURCE=base(4,3,3)(strList( 3 ),rA( 7:9 )))

    item7 = base(4,3,3)(strList( 7 ),rA( 19:21 ))
    item8 = base(4,3,3)(strList( 8 ),rA( 22:24 ))
    item9 = base(4,3,3)(strList( 9 ),rA( 25:27 ))


    ALLOCATE(baseA( 9 ), STAT=stat, ERRMSG=errmsg,&
                SOURCE=[    [ item1, item2, item3 ],&
                            [   base(4,3,3)(strList( 4 ),rA( 10:12 )),&
                                base(4,3,3)(strList( 5 ),rA( 13:15 )),&
                                base(4,3,3)(strList( 6 ),rA( 16:18 ))    ],&
                            [ item7, item8, item9 ] ])

    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(', stat, ') ', errmsg
        CALL zzrc( 10_4 )
    END IF


    IF (SIZE( baseA ) /= 9)         CALL zzrc( 20_4 )

    DO i = 1, SIZE( baseA )
        j = ((i - 1) * 3) + 1

        IF (baseA( i ) /= base(4,3,3)(strList( i ),rA( j:(j + 2) )))&
                                            CALL zzrc( (20_4 + INT(i, 4)) )
    END DO


    rc = VerifyActualArg(                                           &
            [                                                       &
                [   baseA( 9 ), baseA( 8 ), baseA( 7 ),             &
                    [   base(4,3,3)(strList( 6 ),rA( 16:18 )),      &
                        base(4,3,3)(strList( 5 ),rA( 13:15 )),      &
                        [   base(4,3,3)(strList( 4 ),rA( 10:12 )),  &
                            baseA( 3 ), baseA( 2 ), baseA( 1 )      &
                        ]                                           &
                    ]                                               &
                ]                                                   &
            ],                                                      &
                            30_4)

    IF (rc /= 0_4) CALL zzrc( rc )

END PROGRAM aceNestedActualArg01


INTEGER(4) FUNCTION VerifyActualArg(array, rc)
    USE bMod

    IMPLICIT NONE

    TYPE(base(4,*,*)) :: array( : )
    INTEGER(4) :: rc


    INTEGER :: i
    INTEGER :: j


    VerifyActualArg = 0_4
    IF (SIZE( array ) /= 9) THEN
        VerifyActualArg = rc

    ELSE

        i = 0
        DO WHILE ((i < SIZE( array ))  .AND.  VerifyActualArg == 0_4)
            i = i + 1
            j = 9 - i + 1

            IF (array( i ) /= baseA( j )) THEN
                VerifyActualArg = rc + INT(i, 4)
            END IF
        END DO

    END IF

END FUNCTION VerifyActualArg
