!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : ofsk06f.f
!*
!*  PROGRAMMER                 : Jin Li
!*  DATE                       : 2010-09-30
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  Two internal subroutines both are terminated by "END" 
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      PROGRAM ofsk06f
C***********************************************************************
C*
C*   INITIAL DECLARATIONS
C*
C***********************************************************************

      INTEGER CASENUM, I
      integer start,stop
      integer chlen

      integer  i5(5),i312(-2:0,1,2),i5b(5)
      integer, target :: Arg2t(-2:0,1,2)

      integer,parameter :: p_i6(-7:-5,0:1) = 
     + RESHAPE( (/ 1,2,3,4,5,7 /), (/ 3,2 /) )

      character*7 h32(3,2)
      character*5 h5(-2:2)

       
C***********************************************************************
C*
C*   Test A: Integer Arguments
C*
C***********************************************************************

      i5b = (/ 1,5,2,4,3 /)

      !
      !- Whole Arrays as actual arguments
      !
      CASENUM = 10
      i5 = (/ 1,2,3,4,5 /)
      i312 = RESHAPE( (/ 1,2,3,4,5,6 /), (/ 3,1,2 /) )
      start = -2 
      stop = 2
      CALL SUB1( i5,i312 )
      IF (
     +( i5(1) .NE. 2 ) .OR.
     +( i5(2) .NE. 3 ) .OR.
     +( i5(3) .NE. 4 ) .OR.
     +( i5(4) .NE. 5 ) .OR.
     +( i5(5) .NE. 6 ) )
     +CALL ZZRC( CASENUM )
      IF (
     +( i312(-2,1,1) .NE. 2 ) .OR.
     +( i312(-1,1,1) .NE. 3 ) .OR.
     +( i312( 0,1,1) .NE. 4 ) .OR.
     +( i312(-2,1,2) .NE. 5 ) .OR.
     +( i312(-1,1,2) .NE. 6 ) .OR.
     +( i312( 0,1,2) .NE. 7 ) )
     +CALL ZZRC( CASENUM )

      !
      !- Array Sections
      !
      CASENUM = 20
      i5 = (/ 5,4,3,2,1 /)
      i312 = RESHAPE( (/ 3,2,1,6,5,4 /), (/ 3,1,2 /) )
      start = -2 
      stop = 2
      CALL SUB1( i5(5:1:-1),
     +i312(ubound(i312,1):lbound(i312,1):-1,:,:) )
      IF (
     +( i5(1) .NE. 6 ) .OR.
     +( i5(2) .NE. 5 ) .OR.
     +( i5(3) .NE. 4 ) .OR.
     +( i5(4) .NE. 3 ) .OR.
     +( i5(5) .NE. 2 ) )
     +CALL ZZRC( CASENUM )
      IF (
     +( i312(-2,1,1) .NE. 4 ) .OR.
     +( i312(-1,1,1) .NE. 3 ) .OR.
     +( i312( 0,1,1) .NE. 2 ) .OR.
     +( i312(-2,1,2) .NE. 7 ) .OR.
     +( i312(-1,1,2) .NE. 6 ) .OR.
     +( i312( 0,1,2) .NE. 5 ) )
     +CALL ZZRC( CASENUM )

      !
      !- Array Vector Subscripts
      !
      CASENUM = 30
      i5 = (/ 1,5,2,4,3 /)
      i312 = RESHAPE( (/ 1,3,2,4,6,5 /), (/ 3,1,2 /) )
      start = -2 
      stop = 2
      CALL SUB1( i5((/1,3,5,4,2/)),
     +i312((/-2,0,-1/),(/1/),(/1,2/)) )
      !
      !- Check That vector subscript sections do Not change their values
      !
      IF (
     +( i5(1) .NE. 1 ) .OR.
     +( i5(2) .NE. 5 ) .OR.
     +( i5(3) .NE. 2 ) .OR.
     +( i5(4) .NE. 4 ) .OR.
     +( i5(5) .NE. 3 ) )
     +CALL ZZRC( CASENUM )

      !
      !- Array Constructors As Actual Arguments
      !
      CASENUM = 40
      start = -2 
      stop = 2
      CALL SUB1( (/ 1,2,3,4,5 /), 
     + RESHAPE( (/ 1,2,3,4,5,6 /), (/ 3,1,2 /) ) )

C***********************************************************************
C*
C*   Test B: Character Arguments
C*
C***********************************************************************

      chlen = 7
      start = -1
      stop = 1

      !
      !- Whole Array
      !
      CASENUM = 50
      h5 = (/ 'aaaaa','bbbbb','ccccc','ddddd','eeeee' /)
      h32 = RESHAPE( (/ 'a1a2a3a','b1b2b3b','c1c2c3c',
     + 'd1d2d3d','e1e2e3e','f1f2f3f' /), (/ 3,2 /) )
 
      CALL sub2( h5, h32 )
      IF (
     +( h5( 2) .NE. 'eeeee' ) .OR.
     +( h5( 1) .NE. 'ddddd' ) .OR.
     +( h5( 0) .NE. 'ccccc' ) .OR.
     +( h5(-1) .NE. 'bbbbb' ) .OR.
     +( h5(-2) .NE. 'aaaaa' ) ) 
     +CALL ZZRC( CASENUM )
      IF (
     +( h32(1,1) .NE. 'aaaaa  ' ) .OR.
     +( h32(2,1) .NE. 'bbbbb  ' ) .OR.
     +( h32(3,1) .NE. 'ccccc  ' ) .OR.
     +( h32(1,2) .NE. 'ddddd  ' ) .OR.
     +( h32(2,2) .NE. 'eeeee  ' ) .OR.
     +( h32(3,2) .NE. 'f1f2f3f' ) )
     +CALL ZZRC( CASENUM )

      !
      !- Array Section
      !
      CASENUM = 60
      h5 = (/ 'eeeee','ddddd','ccccc','bbbbb','aaaaa' /)
      h32 = RESHAPE( (/ 'a1a2a3a','b1b2b3b','c1c2c3c',
     + 'd1d2d3d','e1e2e3e','f1f2f3f' /), (/ 3,2 /) )
 
      CALL sub2( h5(2:-2:-1), h32(:,:) )
      IF (
     +( h5( 2) .NE. 'aaaaa' ) .OR.
     +( h5( 1) .NE. 'bbbbb' ) .OR.
     +( h5( 0) .NE. 'ccccc' ) .OR.
     +( h5(-1) .NE. 'ddddd' ) .OR.
     +( h5(-2) .NE. 'eeeee' ) ) 
     +CALL ZZRC( CASENUM )
      IF (
     +( h32(1,1) .NE. 'aaaaa  ' ) .OR.
     +( h32(2,1) .NE. 'bbbbb  ' ) .OR.
     +( h32(3,1) .NE. 'ccccc  ' ) .OR.
     +( h32(1,2) .NE. 'ddddd  ' ) .OR.
     +( h32(2,2) .NE. 'eeeee  ' ) .OR.
     +( h32(3,2) .NE. 'f1f2f3f' ) )
     +CALL ZZRC( CASENUM )

      !
      !- Array Constructor
      !
      CASENUM = 70
      h5 = (/ 'aaaaa','bbbbb','ccccc','ddddd','eeeee' /)
      h32 = RESHAPE( (/ 'a1a2a3a','b1b2b3b','c1c2c3c',
     + 'd1d2d3d','e1e2e3e','f1f2f3f' /), (/ 3,2 /) )
 
      CALL sub2( (/ 'aaaaa','bbbbb','ccccc','ddddd','eeeee' /),
     +h32 )


C***********************************************************************
C*
C*   Start of Internal Subroutines
C*
C***********************************************************************

      contains

C***********************************************************************
C*
C*   SUBROUTINE OF INTEGER ARGUMENTS
C*
C***********************************************************************

      ! 
      !-  Explicit Shape Subroutine
      !
      subroutine sub1( arg1, arg2 )
         integer arg1( -2:2 )
         integer arg2(3,1,2)
         integer test(6)
         !
         !- start, stop are host associated
         !  
         integer hosttest(start:stop)
  
         if (
     +   ( arg1(-2) .NE. 1 ) .OR.
     +   ( arg1(-1) .NE. 2 ) .OR.
     +   ( arg1( 0) .NE. 3 ) .OR.
     +   ( arg1( 1) .NE. 4 ) .OR.
     +   ( arg1( 2) .NE. 5 ) )
     +   CALL ZZRC( CASENUM )

         IF (
     +   ( arg2(1,1,1) .NE. 1 ) .OR.
     +   ( arg2(2,1,1) .NE. 2 ) .OR.
     +   ( arg2(3,1,1) .NE. 3 ) .OR.
     +   ( arg2(1,1,2) .NE. 4 ) .OR.
     +   ( arg2(2,1,2) .NE. 5 ) .OR.
     +   ( arg2(3,1,2) .NE. 6 ) )
     +   CALL ZZRC( CASENUM )

         !
         !- Try Constructor of dummy argument
         !
         CASENUM = CASENUM + 1
         test = 0
         !
         !- i5b is host associated
         !
         test = (/ i5b,6 /)
         IF (
     +   ( test(1) .NE. 1 ) .OR.
     +   ( test(2) .NE. 5 ) .OR.
     +   ( test(3) .NE. 2 ) .OR.
     +   ( test(4) .NE. 4 ) .OR.
     +   ( test(5) .NE. 3 ) .OR.
     +   ( test(6) .NE. 6 ) )
     +   CALL ZZRC( CASENUM )
         
         !
         !- Try Constructor of dummy argument
         !
         CASENUM = CASENUM + 1
         test = 0
         test = (/ arg1((/i5b-3/)),6 /)
         IF (
     +   ( test(1) .NE. 1 ) .OR.
     +   ( test(2) .NE. 5 ) .OR.
     +   ( test(3) .NE. 2 ) .OR.
     +   ( test(4) .NE. 4 ) .OR.
     +   ( test(5) .NE. 3 ) .OR.
     +   ( test(6) .NE. 6 ) )
     +   CALL ZZRC( CASENUM )
         
         !
         !- Try Constructor of dummy argument
         !
         CASENUM = CASENUM + 1
         test = 0
         test = (/ arg2 /)
         IF (
     +   ( test(1) .NE. 1 ) .OR.
     +   ( test(2) .NE. 2 ) .OR.
     +   ( test(3) .NE. 3 ) .OR.
     +   ( test(4) .NE. 4 ) .OR.
     +   ( test(5) .NE. 5 ) .OR.
     +   ( test(6) .NE. 6 ) )
     +   CALL ZZRC( CASENUM )
         
         !
         !- Try Section of dummy arg
         !
         CASENUM = CASENUM + 1
         test = 0
         test = arg1( (/ -2,2,-1,1,0,0 /) )
         IF (
     +   ( test(1) .NE. 1 ) .OR.
     +   ( test(2) .NE. 5 ) .OR.
     +   ( test(3) .NE. 2 ) .OR.
     +   ( test(4) .NE. 4 ) .OR.
     +   ( test(5) .NE. 3 ) .OR.
     +   ( test(6) .NE. 3 ) )
     +   CALL ZZRC( CASENUM )
         
         !
         !- Try Section of dummy arg
         !
         CASENUM = CASENUM + 1
         test = 0
         test = (/ arg2(3:1:-1,:,:) /)
         IF (
     +   ( test(1) .NE. 3 ) .OR.
     +   ( test(2) .NE. 2 ) .OR.
     +   ( test(3) .NE. 1 ) .OR.
     +   ( test(4) .NE. 6 ) .OR.
     +   ( test(5) .NE. 5 ) .OR.
     +   ( test(6) .NE. 4 ) )
     +   CALL ZZRC( CASENUM )

         !
         !- Set the host associated automatic array
         !
         CASENUM = CASENUM + 1
         !
         !- changing start and stop should NOT affect the bounds
         !- of the array hosttest
         !
         start = 1
         stop = 1
         hosttest = arg1
         if (
     +   ( hosttest(-2) .NE. 1 ) .OR.
     +   ( hosttest(-1) .NE. 2 ) .OR.
     +   ( hosttest( 0) .NE. 3 ) .OR.
     +   ( hosttest( 1) .NE. 4 ) .OR.
     +   ( hosttest( 2) .NE. 5 ) )
     +   CALL ZZRC( CASENUM )

         !
         !- Try Section of Host Associated Named Constant Array
         !
         CASENUM = CASENUM + 1
         test = 0
         test = (/ p_i6(ubound(p_i6,1):lbound(p_i6,1):-1,:) /)
         IF (
     +   ( lbound( p_i6, 1 ) .NE. -7 ) .OR.
     +   ( lbound( p_i6, 2 ) .NE.  0 ) .OR.
     +   ( ubound( p_i6, 1 ) .NE. -5 ) .OR.
     +   ( ubound( p_i6, 2 ) .NE.  1 ) .OR.
     +   ( test(1) .NE. 3 ) .OR.
     +   ( test(2) .NE. 2 ) .OR.
     +   ( test(3) .NE. 1 ) .OR.
     +   ( test(4) .NE. 7 ) .OR.
     +   ( test(5) .NE. 5 ) .OR.
     +   ( test(6) .NE. 4 ) )
     +   CALL ZZRC( CASENUM )

         arg1 = arg1 + 1
         arg2 = arg2 + 1

      end 
 
C***********************************************************************
C*
C*   SUBROUTINE OF character ARGUMENTS
C*
C***********************************************************************

      subroutine sub2( Arg1, Arg2 )
         !
         !- chlen is host associated
         !
         character*5 Arg1(0:4)
         character (len=chlen) Arg2(900:902,2:3)

         character*10 test(5)
         character (len=chlen) :: hosttest(start:stop,start:stop-1)

         IF (
     +   ( Arg1(0) .NE. 'aaaaa' ) .OR.
     +   ( Arg1(1) .NE. 'bbbbb' ) .OR.
     +   ( Arg1(2) .NE. 'ccccc' ) .OR.
     +   ( Arg1(3) .NE. 'ddddd' ) .OR.
     +   ( Arg1(4) .NE. 'eeeee' ) )
     +   CALL ZZRC( CASENUM )

         if (
     +   ( Arg2(900,2) .NE. 'a1a2a3a' ) .OR.
     +   ( Arg2(901,2) .NE. 'b1b2b3b' ) .OR.
     +   ( Arg2(902,2) .NE. 'c1c2c3c' ) .OR.
     +   ( Arg2(900,3) .NE. 'd1d2d3d' ) .OR.
     +   ( Arg2(901,3) .NE. 'e1e2e3e' ) .OR.
     +   ( Arg2(902,3) .NE. 'f1f2f3f' ) )
     +   CALL ZZRC( CASENUM )

         !
         !- Use Arg1 in constructor
         !
         CASENUM = CASENUM + 1
         test = '          '
         test = (/ Arg1 // 'hello' /) 
         IF (
     +   ( TEST(1) .NE. 'aaaaahello' ) .OR.
     +   ( TEST(2) .NE. 'bbbbbhello' ) .OR.
     +   ( TEST(3) .NE. 'ccccchello' ) .OR.
     +   ( TEST(4) .NE. 'dddddhello' ) .OR.
     +   ( TEST(5) .NE. 'eeeeehello' ) )
     +   CALL ZZRC( CASENUM )

         !
         !- Use Sections 
         !
         CASENUM = CASENUM + 1
         test = '          '
         !
         !- Runtime bounds on the substring of the section
         !
         test = (/ Arg1(:3) //
     +   (/  Arg2(900:902:2,2:3)(chlen-4:chlen) /),
     +   'fifthfifth' /) 
         IF (
     +   ( TEST(1) .NE. 'aaaaaa2a3a' ) .OR.
     +   ( TEST(2) .NE. 'bbbbbc2c3c' ) .OR.
     +   ( TEST(3) .NE. 'cccccd2d3d' ) .OR.
     +   ( TEST(4) .NE. 'dddddf2f3f' ) .OR.
     +   ( TEST(5) .NE. 'fifthfifth' ) )
     +   CALL ZZRC( CASENUM )

         !
         !- Use Arg2 in constructor
         !
         CASENUM = CASENUM + 1
         hosttest  = '       '
         hosttest = Arg2
         if (
     +   ( hosttest(-1,-1) .NE. 'a1a2a3a' ) .OR.
     +   ( hosttest( 0,-1) .NE. 'b1b2b3b' ) .OR.
     +   ( hosttest( 1,-1) .NE. 'c1c2c3c' ) .OR.
     +   ( hosttest(-1, 0) .NE. 'd1d2d3d' ) .OR.
     +   ( hosttest( 0, 0) .NE. 'e1e2e3e' ) .OR.
     +   ( hosttest( 1, 0) .NE. 'f1f2f3f' ) )
     +   CALL ZZRC( CASENUM )
         
         CASENUM = CASENUM + 1
         Arg2 = RESHAPE((/ Arg1 // '  ', Arg2(902,3) /),
     +    (/ 3,2 /))

      end 

      end

