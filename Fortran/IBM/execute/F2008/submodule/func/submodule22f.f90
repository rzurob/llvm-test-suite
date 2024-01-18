!**********************************************************************
!*
!*  TEST CASE NAME             : submodule22f
!*
!*  DATE                       : 25 April 2013
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  KEYWORD(S)                 : F2008 SUBMODULE AC IMPLIED DO
!*
!*  TARGET(S)                  :
!*
!*  DESCRIPTION                :
!*  based on arr_misc/constructor/fxco0010
!*
!*  Verify acceptance of array constructors in a submodule and the
!*   scope of an AC implied do, array segments, array expressions, or
!*   a combination.
!*
!*  Secondary tests:
!*  - two submodules may have the same name, as long as they have
!*    different host modules.
!*  - one of these modules use associates the other
!*
!*  Verify that the results match the values of the original test case
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!* ===================================================================
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

!***********************************************************************
!*
!*   INITIAL DECLARATIONS
!*
!***********************************************************************
MODULE DATA
  IMPLICIT NONE
  INTEGER CASENUM,i,j

  INTEGER i10(10),i5a(5),i5b(5)
  INTEGER i55(5,5),i15(-7:7)

  COMPLEX c232(2,3,2),c25(2,5),c210(2,10)

  INTERFACE

    MODULE SUBROUTINE SUB1( Arg1, Arg2, Arg3, CASENUM )
      INTEGER CASENUM
      INTEGER Arg1(:), Arg2(:), Arg3(:)
    END SUBROUTINE SUB1

    MODULE SUBROUTINE SUB2( Arg1, Arg2, Arg3, CASENUM )
      INTEGER CASENUM
        COMPLEX Arg1(:,:,CASENUM:),Arg2(:,:),Arg3(:,:)
      END SUBROUTINE SUB2

      MODULE SUBROUTINE SUB3( Arg1, Arg2, CASENUM )
        INTEGER CASENUM
        character*5  Arg1(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        character*5  Arg2(0:,0:,0:)
      END SUBROUTINE SUB3

  END INTERFACE
END MODULE

MODULE TESTS
  USE DATA
  IMPLICIT NONE

  INTERFACE
    MODULE SUBROUTINE TEST1()
    END SUBROUTINE

    MODULE SUBROUTINE TEST2()
    END SUBROUTINE

    MODULE SUBROUTINE TEST3()
    END SUBROUTINE

    MODULE SUBROUTINE TEST4()
    END SUBROUTINE

    MODULE SUBROUTINE TEST5()
    END SUBROUTINE

    MODULE SUBROUTINE TEST6()
    END SUBROUTINE

    MODULE FUNCTION RUNTESTS()
      LOGICAL RUNTESTS
    END FUNCTION
  END INTERFACE
CONTAINS

  MODULE FUNCTION RUNTESTS()
    LOGICAL RUNTESTS
    CALL TEST1()
    CALL TEST2()
    CALL TEST3()
    CALL TEST4()
    CALL TEST5()
    CALL TEST6()
    RUNTESTS = .TRUE.
  END FUNCTION
END MODULE TESTS

SUBMODULE (TESTS) SUBMOD
CONTAINS
!***********************************************************************
!*
!* Call Sub1 with Single Dimensional Arrays
!*
!* (/1,2,3,4,5,6,7,8,9,10/), (/3,4,5,6,7/), (/1,3,5,7,9/)
!*
!***********************************************************************
  !- Whole Arrays
  MODULE SUBROUTINE TEST1()
    i10 = (/ 1,2,3,4,5,6,7,8,9,10 /)
    i5a = (/ 3,4,5,6,7 /)
    i5b = (/ 1,3,5,7,9 /)

    CASENUM = 1
    call sub1( i10,i5a,i5b,CASENUM )
    if ( ( i10(1).NE.3 ) .OR.( i10(2).NE.4 ) .OR.( i10(3).NE.5 ) &
     .OR.( i10(4).NE.6 ) .OR.( i10(5).NE.7 ) .OR.( i10(6).NE.1 ) &
     .OR.( i10(7).NE.3 ) .OR.( i10(8).NE.5 ) .OR.( i10(9).NE.7 ) &
     .OR.( i10(10).NE.9 ) )  CALL ZZRC( CASENUM )
   END SUBROUTINE

  !- Array Sections
  MODULE SUBROUTINE TEST2()

    i15 = (/ (i,i=-2,12) /)
    i55(:,3) = (/ 3,4,5,6,7 /)
    i55(3,:) = (/ 1,3,5,7,9 /)

    CASENUM = 10
    call sub1( i15(-4:5), i55(:,3), i55(3,:), CASENUM )
    if ( ( i15(-4).NE.3 ) .OR.( i15(-3).NE.4 ) .OR.( i15(-2).NE.5 ) &
     .OR.( i15(-1).NE.6 ) .OR.( i15(-0).NE.7 ) .OR.( i15( 1).NE.1 ) &
     .OR.( i15( 2).NE.3 ) .OR.( i15( 3).NE.5 ) .OR.( i15( 4).NE.7 ) &
     .OR.( i15( 5) .NE. 9 ) ) CALL ZZRC( CASENUM )
  END SUBROUTINE

  !- Array Vector Subscript Section
  MODULE SUBROUTINE TEST3()
    i10 = (/ 1,10,2,9,3,8,4,7,5,6 /)
    i5a = (/ 3,4,5,6,7 /)
    i5b = (/ 1,5,9,8,4 /)

    CASENUM = 20
    call sub1( i10((/(i,i=1,10,2),(i,i=10,2,-2) /)),i5a,i10( (/i5b/) ), CASENUM )
  END SUBROUTINE

  !- Array Constructors
  MODULE SUBROUTINE TEST4()
    CASENUM = 30
    call sub1( (/(CASENUM,CASENUM=1,10)/), (/3,4,5,6,7/),(/1,3,5,7,9/), CASENUM )
  END SUBROUTINE

  !- Array Expressions
  MODULE SUBROUTINE TEST5()
    i10 = (/ 1,1,2,2,3,3,4,4,5,5 /)
    i5a = (/ 1,2,3,4,5 /)
    i5b = (/ 6,7,8,9,10 /)
    i55(:,1) = (/ 5,4,-3,-2,-1 /)

    CASENUM = 40
    call sub1( i10+(/0,1,1,2,2,3,3,4,4,5/), i5a+2,i5b-abs(i55(:,1)), CASENUM )
  END SUBROUTINE

!***********************************************************************
!*
!* Call Sub2 with Multi-Dimensional Arrays
!*
!* (6,7,4,5,8,9,1,10,2,9,3,8) as (2,3,2)
!* (1,2,3,4,5,6,7,8,9,10)     as (2,5)
!* (1,10,2,9,3,8,4,7,5,6,6,5,7,4,8,3,9,2,10,1) as (2,10)
!*
!***********************************************************************

  MODULE SUBROUTINE TEST6()
    c232 = RESHAPE( (/ (6.0,6.0),(7.0,7.0),(4.0,4.0),(5.0,5.0),(8.0,8.0),(9.0,9.0), (1.0,1.0),(10.0,10.0),(2.0,2.0),(9.0,9.0),(3.0,3.0),(8.0,8.0) /), (/ 2,3,2 /) )
    c25 = RESHAPE( (/ (1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0),(5.0,5.0),(6.0,6.0),(7.0,7.0),(8.0,8.0),(9.0,9.0),(10.0,10.0) /), (/2,5/) )
    c210(1,:) = (/ (1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0),(5.0,5.0),(6.0,6.0),(7.0,7.0),(8.0,8.0),(9.0,9.0),(10.0,10.0) /)
    c210(2,10:1:-1) = c210(1,:)

    CASENUM = 50
    Call sub2( c232,c25,c210, CASENUM )

  END SUBROUTINE
END SUBMODULE

!***********************************************************************
!*
!*   Test A: Test assumed shape dummy array in assignment constructors.
!*           Single Dimensional Array Usage
!*
!***********************************************************************
SUBMODULE (DATA) SUBMOD
CONTAINS
!
!- Test arrays within constructors.
!- Arg1 is of size 10 = 1,2,3,4,5,6,7,8,9,10
!- Arg2 is of size 5  = 3,4,5,6,7
!- Arg3 is of size 5  = 1,3,5,7,9
!
  MODULE SUBROUTINE SUB1( Arg1, Arg2, Arg3, CASENUM )
    INTEGER CASENUM
    INTEGER Arg1(:), Arg2(:), Arg3(:)
    INTEGER test( 10 )

    CASENUM = CASENUM + 1
    !- Whole Array
    test = (/ Arg1 /)
    if (( test(1).NE.1 ) .OR.( test(2).NE.2 ) .OR.( test(3).NE.3 ) &
    .OR.( test(4).NE.4 ) .OR.( test(5).NE.5 ) .OR.( test(6).NE.6 ) &
    .OR.( test(7).NE.7 ) .OR.( test(8).NE.8 ) .OR.( test(9).NE.9 ) &
    .OR.( test(10) .NE. 10 ) ) CALL ZZRC( CASENUM )

    CASENUM = CASENUM + 1
    !- Array Section
    test = 0
    test = (/ 1,2,Arg1(3:8),9,10 /)
    if (( test(1).NE.1 ) .OR.( test(2).NE.2 ) .OR.( test(3).NE.3 ) &
    .OR.( test(4).NE.4 ) .OR.( test(5).NE.5 ) .OR.( test(6).NE.6 ) &
    .OR.( test(7).NE.7 ) .OR.( test(8).NE.8 ) .OR.( test(9).NE.9 ) &
    .OR.( test(10).NE.10 ) )  CALL ZZRC( CASENUM )

    CASENUM = CASENUM + 1
    !- Array Section (Assumed End of assumed shape array)
    test = 0
    test = (/ (/ 1,2,Arg2(:)/),Arg1(8:) /)
    if ( ( test(1).NE.1 ) .OR.( test(2).NE.2 ) .OR.( test(3).NE.3 ) &
     .OR.( test(4).NE.4 ) .OR.( test(5).NE.5 ) .OR.( test(6).NE.6 ) &
     .OR.( test(7).NE.7 ) .OR.( test(8).NE.8 ) .OR.( test(9).NE.9 ) &
     .OR.( test(10) .NE. 10 ) )  CALL ZZRC( CASENUM )

    CASENUM = CASENUM + 1
    !- Vector Subscript
    test = 0
    test((/(CASENUM,CASENUM = 1,9,2)/)) = (/ Arg1((/Arg3/)) /)
    test( (/Arg3+1/) ) = (/ 2,4,6,8,10 /)
    if ( ( test(1).NE.1 ) .OR.( test(2).NE.2 ) .OR.( test(3).NE.3 ) &
     .OR.( test(4).NE.4 ) .OR.( test(5).NE.5 ) .OR.( test(6).NE.6 ) &
     .OR.( test(7).NE.7 ) .OR.( test(8).NE.8 ) .OR.( test(9).NE.9 ) &
     .OR.( test(10).NE.10 ) ) CALL ZZRC( CASENUM )

    CASENUM = CASENUM + 1
    !- Within Constructor Expressions
    test = 0
    test = (/ Arg3+(/0,-1,-2,-3,-4/),6,7,8,9,10 /)
    if ( ( test(1).NE.1 ) .OR.( test(2).NE.2 ) .OR.( test(3).NE.3 ) &
     .OR.( test(4).NE.4 ) .OR.( test(5).NE.5 ) .OR.( test(6).NE.6 ) &
     .OR.( test(7).NE.7 ) .OR.( test(8).NE.8 ) .OR.( test(9).NE.9 ) &
     .OR.( test(10).NE.10 ) )   CALL ZZRC( CASENUM )

    CASENUM = CASENUM + 1
    !- Expression with a Constant
    test = 0
    test = (/ Arg2-2, Arg1(6:10) /)
    if ( ( test(1).NE.1 ) .OR.( test(2).NE.2 ) .OR.( test(3).NE.3 ) &
     .OR.( test(4).NE.4 ) .OR.( test(5).NE.5 ) .OR.( test(6).NE.6 ) &
     .OR.( test(7).NE.7 ) .OR.( test(8).NE.8 ) .OR.( test(9).NE.9 ) &
     .OR.( test(10).NE.10 ) ) CALL ZZRC( CASENUM )

    CASENUM = CASENUM + 1
    !- Complex Constructor
    test = 0
    test = (/ 1,(/2,Arg2/),(/Arg1(8:10)/) /)
    if ( ( test(1).NE.1 ) .OR.( test(2).NE.2 ) .OR.( test(3).NE.3 ) &
     .OR.( test(4).NE.4 ) .OR.( test(5).NE.5 ) .OR.( test(6).NE.6 ) &
     .OR.( test(7).NE.7 ) .OR.( test(8).NE.8 ) .OR.( test(9).NE.9 ) &
     .OR.( test(10) .NE. 10 ) ) CALL ZZRC( CASENUM )

    Arg1 = (/ Arg2,Arg3 /)

  END SUBROUTINE SUB1

!***********************************************************************
!*
!*   Test B: Test assumed shape dummy array in assignment constructors.
!*           Single Dimenion Usage of Multi-Dimensional Arrays
!*
!***********************************************************************

  MODULE SUBROUTINE SUB2( Arg1, Arg2, Arg3, CASENUM )
    INTEGER CASENUM
    COMPLEX Arg1(:,:,CASENUM:),Arg2(:,:),Arg3(:,:)
    COMPLEX test(10)
    logical precision_x8
    integer j

    j = CASENUM

    !- NOTE!!! CASENUM is redefined, but Arg1 should NOT be

    !- Whole Array
    test = (0.0,0.0)
    test = (/ Arg3(1,:) /)
    CASENUM = CASENUM + 1
    IF ( ( .NOT. precision_x8( test(1), (1.0,1.0) )) .OR. &
         ( .NOT. precision_x8( test(2), (2.0,2.0) )) .OR. &
         ( .NOT. precision_x8( test(3), (3.0,3.0) )) .OR. &
         ( .NOT. precision_x8( test(4), (4.0,4.0) )) .OR. &
         ( .NOT. precision_x8( test(5), (5.0,5.0) )) .OR. &
         ( .NOT. precision_x8( test(6), (6.0,6.0) )) .OR. &
         ( .NOT. precision_x8( test(7), (7.0,7.0) )) .OR. &
         ( .NOT. precision_x8( test(8), (8.0,8.0) )) .OR. &
         ( .NOT. precision_x8( test(9), (9.0,9.0) )) .OR. &
         ( .NOT. precision_x8( test(10), (10.0,10.0) )) ) &
         CALL ZZRC( CASENUM )

    !- Array Section
    CASENUM = CASENUM + 1
    test = (0.0,0.0)

    test = (/ Arg1(1,:,j+1),Arg1(:,2,j),Arg1(:,1,j), Arg1(2,3:1:-1,j+1) /)
    IF ( ( .NOT. precision_x8( test(1), (1.0,1.0) )) .OR. &
         ( .NOT. precision_x8( test(2), (2.0,2.0) )) .OR. &
         ( .NOT. precision_x8( test(3), (3.0,3.0) )) .OR. &
         ( .NOT. precision_x8( test(4), (4.0,4.0) )) .OR. &
         ( .NOT. precision_x8( test(5), (5.0,5.0) )) .OR. &
         ( .NOT. precision_x8( test(6), (6.0,6.0) )) .OR. &
         ( .NOT. precision_x8( test(7), (7.0,7.0) )) .OR. &
         ( .NOT. precision_x8( test(8), (8.0,8.0) )) .OR. &
         ( .NOT. precision_x8( test(9), (9.0,9.0) )) .OR. &
         ( .NOT. precision_x8( test(10), (10.0,10.0) )) ) &
         CALL ZZRC( CASENUM )

    !- Array Vector Subscript
    CASENUM = CASENUM + 1
    test = (0.0,0.0)
    test = (/ (1.0,1.0),(2.0,2.0),(3.0,3.0), Arg1(:,(/2,1,3/),j),(10.0,10.0) /)
    IF ( ( .NOT. precision_x8( test(1), (1.0,1.0) )) .OR. &
         ( .NOT. precision_x8( test(2), (2.0,2.0) )) .OR. &
         ( .NOT. precision_x8( test(3), (3.0,3.0) )) .OR. &
         ( .NOT. precision_x8( test(4), (4.0,4.0) )) .OR. &
         ( .NOT. precision_x8( test(5), (5.0,5.0) )) .OR. &
         ( .NOT. precision_x8( test(6), (6.0,6.0) )) .OR. &
         ( .NOT. precision_x8( test(7), (7.0,7.0) )) .OR. &
         ( .NOT. precision_x8( test(8), (8.0,8.0) )) .OR. &
         ( .NOT. precision_x8( test(9), (9.0,9.0) )) .OR. &
         ( .NOT. precision_x8( test(10), (10.0,10.0) )) ) &
         CALL ZZRC( CASENUM )

    !- Expressions within Constructor
    CASENUM = CASENUM + 1
    test = (0.0,0.0)
    test = (/ Arg2(1,:)+ (/(0.0,0.0),(-1.0,-1.0),(-2.0,-2.0),(-3.0,-3.0),(-4.0,-4.0)/), Arg3(2,6:2:-1)+(1.0,1.0) /)
    IF ( ( .NOT. precision_x8( test(1), (1.0,1.0) )) .OR. &
         ( .NOT. precision_x8( test(2), (2.0,2.0) )) .OR. &
         ( .NOT. precision_x8( test(3), (3.0,3.0) )) .OR. &
         ( .NOT. precision_x8( test(4), (4.0,4.0) )) .OR. &
         ( .NOT. precision_x8( test(5), (5.0,5.0) )) .OR. &
         ( .NOT. precision_x8( test(6), (6.0,6.0) )) .OR. &
         ( .NOT. precision_x8( test(7), (7.0,7.0) )) .OR. &
         ( .NOT. precision_x8( test(8), (8.0,8.0) )) .OR. &
         ( .NOT. precision_x8( test(9), (9.0,9.0) )) .OR. &
         ( .NOT. precision_x8( test(10), (10.0,10.0) )) ) &
         CALL ZZRC( CASENUM )

  END SUBROUTINE SUB2

!***********************************************************************
!*
!*   Test C: Test assumed shape dummy array in assignment constructors.
!*           Multi-Dimensional Arrays linearized within Constructors
!*
!***********************************************************************

  MODULE SUBROUTINE SUB3( Arg1, Arg2, CASENUM )
    INTEGER CASENUM
      character*5  Arg1(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
      character*5  Arg2(0:,0:,0:)
      character*5 test(8)

      !- Whole Array linearized ( 20-dimensional array )
      CASENUM = CASENUM + 1
      test = (/ Arg1 /)
      IF (( test(1) .NE. 'HIaaa' ) .OR. ( test(2) .NE. 'HIbbb' ) .OR. &
          ( test(3) .NE. 'HIccc' ) .OR.( test(4) .NE. 'HIddd' ) .OR.  &
          ( test(5) .NE. 'HIeee' ) .OR.( test(6) .NE. 'HIfff' ) .OR.  &
          ( test(7) .NE. 'HIggg' ) .OR. ( test(8) .NE. 'HIhhh' ) )    &
          CALL ZZRC( CASENUM )

      !- Array Section
      CASENUM = CASENUM + 1
      test = (/ Arg1(:,1,:,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,:) /)
      IF (( test(1) .NE. 'HIaaa' ) .OR. ( test(2) .NE. 'HIbbb' ) .OR. &
          ( test(3) .NE. 'HIccc' ) .OR. ( test(4) .NE. 'HIddd' ) .OR. &
          ( test(5) .NE. 'HIeee' ) .OR. ( test(6) .NE. 'HIfff' ) .OR. &
          ( test(7) .NE. 'HIggg' ) .OR. ( test(8) .NE. 'HIhhh' ) )    &
          CALL ZZRC( CASENUM )

      !- Array Section
      CASENUM = CASENUM + 1
      test = (/ Arg2(:,:,2),Arg2(:,:,1) /)
      IF ( ( test(1) .NE. 'HIaaa' ) .OR. ( test(2) .NE. 'HIbbb' ) .OR. &
           ( test(3) .NE. 'HIccc' ) .OR. ( test(4) .NE. 'HIddd' ) .OR. &
           ( test(5) .NE. 'HIeee' ) .OR. ( test(6) .NE. 'HIfff' ) .OR. &
           ( test(7) .NE. 'HIggg' ) .OR. ( test(8) .NE. 'HIhhh' ) )    &
           CALL ZZRC( CASENUM )

      !- Array Expression
      CASENUM = CASENUM + 1
      test = (/ 'HI'//Arg2(:,:,2)(3:5),Arg2(:,:,1) /)
      IF ( ( test(1) .NE. 'HIaaa' ) .OR. ( test(2) .NE. 'HIbbb' ) .OR. &
           ( test(3) .NE. 'HIccc' ) .OR. ( test(4) .NE. 'HIddd' ) .OR. &
           ( test(5) .NE. 'HIeee' ) .OR. ( test(6) .NE. 'HIfff' ) .OR. &
           ( test(7) .NE. 'HIggg' ) .OR. ( test(8) .NE. 'HIhhh' ) )    &
           CALL ZZRC( CASENUM )

      !- Array Expression
      CASENUM = CASENUM + 1
      test = (/ (/ 'HI'//(/'a','b','c','d','e','f','g','h'/)/) // (/ Arg2(:,:,2),Arg2(:,:,1) /) /)
      IF ( ( test(1) .NE. 'HIaaa' ) .OR. ( test(2) .NE. 'HIbbb' ) .OR. &
           ( test(3) .NE. 'HIccc' ) .OR. ( test(4) .NE. 'HIddd' ) .OR. &
           ( test(5) .NE. 'HIeee' ) .OR. ( test(6) .NE. 'HIfff' ) .OR. &
           ( test(7) .NE. 'HIggg' ) .OR. ( test(8) .NE. 'HIhhh' ) )    &
           CALL ZZRC( CASENUM )

  END SUBROUTINE SUB3
END SUBMODULE

PROGRAM SUBMODULE22F
USE TESTS

IF (.not.RUNTESTS()) error stop 66
END
