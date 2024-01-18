!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : diffKindVectorIndex05 - Order of Expression
!*                               Evaluation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 13, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Intrinsic
!*                               Type INTEGER
!*  SECONDARY FUNCTIONS TESTED : expr references elements of variable (Indexed
!*                               using a Vector Subscript of Elements from
!*                               variable), and will have a different Length
!*                               Type Parameter Result
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is an allocated allocatable variable, it is deallocated if
!*  expr is an array of different shape or any of the corresponding length
!*  type parameter values of variable and expr differ. If variable is or
!*  becomes an unallocated allocatable variable, then it is allocated with
!*  each deferred type parameter equal to the corresponding type parameters
!*  of expr, with the shape of expr, and with each lower bound equal to the
!*  corresponding element of LBOUND(expr).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM diffKindVectorIndex05

    INTEGER :: intArr( 10,10,10 ) = RESHAPE(&
        (/ (((k, k = 10, 1, -1), j = 1, 10), i = 1, 10) /), (/ 10,10,10 /))

    INTEGER, ALLOCATABLE :: intArrAlloc( :,:,: )


    intArrAlloc = intArr
    CALL CheckIt( 10_4 )


    intArrAlloc =&
        INT(intArrAlloc( intArrAlloc( 10:1:-1,1,1 ),&
                         intArrAlloc( 10:1:-1,1,1 ),&
                         intArrAlloc( :,1,1 )        ), 8)

    CALL CheckIt( 20_4 )


    intArrAlloc =&
        INT(intArrAlloc( intArrAlloc( 10:1:-1,1,1 ),&
                         intArrAlloc( :,1,1 ),&
                         intArrAlloc( 10:1:-1,1,1 ) ), 4)

    CALL CheckIt( 30_4 )


    intArrAlloc( intArrAlloc( :,1,1 ),&
                 intArrAlloc( 10:1:-1,1,1 ),&
                 intArrAlloc( 10:1:-1,1,1 ) ) =&
        INT(intArrAlloc( intArrAlloc( :,1,1 ),&
                         intArrAlloc( 10:1:-1,1,1 ),&
                         intArrAlloc( 10:1:-1,1,1 )  ), 2)

    CALL CheckIt( 40_4 )


    intArrAlloc( intArrAlloc( :,1,1 ),&
                 intArrAlloc( :,1,1 ),&
                 intArrAlloc( :,1,1 ) ) =&
            intArrAlloc( intArrAlloc( :,1,1 ),&
                         intArrAlloc( :,1,1 ),&
                         intArrAlloc( :,1,1 )  )

    CALL CheckIt( 50_4 )


    CONTAINS


        SUBROUTINE CheckIt( failRCbase )
            INTEGER(4) :: failRCbase


            IF (.NOT. ALLOCATED( intArrAlloc )) CALL zzrc( failRCbase )

            IF (SIZE(intArrAlloc, 1) /= 10)     CALL zzrc( (failRCbase + 1_4) )
            IF (SIZE(intArrAlloc, 2) /= 10)     CALL zzrc( (failRCbase + 2_4) )
            IF (SIZE(intArrAlloc, 3) /= 10)     CALL zzrc( (failRCbase + 3_4) )

            IF (KIND( intArrAlloc ) /= KIND( intArr ))&
                            CALL zzrc( (failRCbase + 4_4) )

            IF (.NOT. ALL(intArrAlloc == intArr))&
                            CALL zzrc( (failRCbase + 5_4) )

        END SUBROUTINE CheckIt

END PROGRAM diffKindVectorIndex05
