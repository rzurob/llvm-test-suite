!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  Eleven internal functions are all terminated by "END"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      PROGRAM  fxip0221
C***********************************************************************
C*
C*   INITIAL DECLARATIONS
C*
C***********************************************************************

      INTEGER CASENUM,I

      LOGICAL precision_r4
      LOGICAL precision_x8

      INTEGER, DIMENSION(5) :: RESULTi5, arr_i5
      INTEGER, DIMENSION(8) :: RESULTi8
      INTEGER, DIMENSION(4,2) :: RESULTi42, arr_i42

      REAL     r1,r

      COMPLEX, DIMENSION(5) :: RESULTc5, arr_c5
      COMPLEX, DIMENSION(-2:0,0:2) :: RESULTc33, arr_c33

      REAL   , DIMENSION(5) :: RESULTr5, arr_r5
      REAL   , DIMENSION(9) :: RESULTr9
      REAL   , DIMENSION(3,3) :: arr_r33

      character*5 RESULTch5(5),arr_ch5(5)

C***********************************************************************
C*
C*   Start of Testcases
C*
C***********************************************************************


C---------------------------------
C-
C- Explicit Shape Dimensioning
C- (passing the Dimension As an
C-  actual Argument)
C-
C-----------------------------------

!
!- Dimension is an Integer Value
!
      CASENUM = 1
      RESULTi5 = (/ 0,0,0,0,0 /)
      arr_i5 = (/ 1,2,3,4,5 /)
      RESULTi5 = explicit1( 5, arr_i5 )
      if (
     +( RESULTi5(1) .NE. 1 ) .OR.
     +( RESULTi5(2) .NE. 2 ) .OR.
     +( RESULTi5(3) .NE. 3 ) .OR.
     +( RESULTi5(4) .NE. 4 ) .OR.
     +( RESULTi5(5) .NE. 5 ) )
     +CALL ZZRC( CASENUM )
!
!- Dimension is an Typeless Constant
!
      CASENUM = 2
      RESULTi5 = (/ 0,0,0,0,0 /)
      arr_i5 = (/ 1,2,3,4,5 /)
      RESULTi5 = explicit1( b"0101", arr_i5 )
      if (
     +( RESULTi5(1) .NE. 1 ) .OR.
     +( RESULTi5(2) .NE. 2 ) .OR.
     +( RESULTi5(3) .NE. 3 ) .OR.
     +( RESULTi5(4) .NE. 4 ) .OR.
     +( RESULTi5(5) .NE. 5 ) )
     +CALL ZZRC( CASENUM )
!
!- Dimension is an Function Result
!
      CASENUM = 3
      RESULTi5 = (/ 0,0,0,0,0 /)
      arr_i5 = (/ 1,2,3,4,5 /)
      RESULTi5 = explicit1( UBOUND(arr_i5,1), arr_i5 )
      if (
     +( RESULTi5(1) .NE. 1 ) .OR.
     +( RESULTi5(2) .NE. 2 ) .OR.
     +( RESULTi5(3) .NE. 3 ) .OR.
     +( RESULTi5(4) .NE. 4 ) .OR.
     +( RESULTi5(5) .NE. 5 ) )
     +CALL ZZRC( CASENUM )
!
!- Dimension in Internal Function is ( -n:1 )
!
      CASENUM = 4
      RESULTi5 = (/ 0,0,0,0,0 /)
      arr_i5 = (/ 1,2,3,4,5 /)
      RESULTi5 = explicit1b( 5, arr_i5 )
      IF (
     +( RESULTi5(1) .NE. 1 ) .OR.
     +( RESULTi5(2) .NE. 2 ) .OR.
     +( RESULTi5(3) .NE. 3 ) .OR.
     +( RESULTi5(4) .NE. 4 ) .OR.
     +( RESULTi5(5) .NE. 5 ) )
     +CALL ZZRC( CASENUM )
!
!- 2 Dimensional Array Dimensioning
!
      CASENUM = 7
!     RESULTi42 = CASENUM-4
      RESULTi42 = 0
      arr_i42 = RESHAPE( (/ 1,2,3,4,5,6,7,8 /), (/ 4,2 /) )
      RESULTi42 = explicit2( 4, 2, arr_i42 )
      if (
     +( RESULTi42(1,1) .NE. 1 ) .OR.
     +( RESULTi42(2,1) .NE. 2 ) .OR.
     +( RESULTi42(3,1) .NE. 3 ) .OR.
     +( RESULTi42(4,1) .NE. 4 ) .OR.
     +( RESULTi42(1,2) .NE. 5 ) .OR.
     +( RESULTi42(2,2) .NE. 6 ) .OR.
     +( RESULTi42(3,2) .NE. 7 ) .OR.
     +( RESULTi42(4,2) .NE. 8 ) )
     +CALL ZZRC( CASENUM )

C---------------------------------
C-
C- Automatic Shape Dimensions
C- (passing the Dimension As an
C-  actual Argument)
C-
C-----------------------------------

!
!- Dimension is an Integer Value
!
      CASENUM = 10
      RESULTc5 = (0.0,0.0)
      arr_c5 = (/ ( (r,r),r=1.0,5.0 ) /)
      RESULTc5 = automatic1( 5, arr_c5 )
      if (
     +( .NOT. precision_x8( RESULTc5(1),(1.0,1.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc5(2),(2.0,2.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc5(3),(3.0,3.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc5(4),(4.0,4.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc5(5),(5.0,5.0) ) ) )
     +CALL ZZRC( CASENUM )
!
!- 2 Dimensional Automatic shape Array Dimesensioning ( of Complex )
!
      CASENUM = 11
      RESULTc33 = (0.0,0.0)
      arr_c33 = RESHAPE( (/ (1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0),
     +(5.0,5.0),(6.0,6.0),(7.0,7.0),(8.0,8.0),(9.0,9.0) /), (/ 3,3 /) )
      RESULTc33 = automati2( 3, 3, arr_c33 )
      if (
     +( .NOT. precision_x8( RESULTc33(-2,0),(1.0,1.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc33(-1,0),(2.0,2.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc33( 0,0),(3.0,3.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc33(-2,1),(4.0,4.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc33(-1,1),(5.0,5.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc33( 0,1),(6.0,6.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc33(-2,2),(7.0,7.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc33(-1,2),(8.0,8.0) ) ) .OR.
     +( .NOT. precision_x8( RESULTc33( 0,2),(9.0,9.0) ) ) )
     +CALL ZZRC( CASENUM )

C---------------------------------
C-
C- Assumed Shape Dimensions
C- (passing the Dimension As an
C-  actual Argument)
C-
C-----------------------------------

!
!- Dimension is an Integer Value ( Try typeless in constructor and as
!- vector subscript section )
!
      CASENUM = 20
      RESULTr5 = 0.0
      arr_r5 = (/ (/ b"00111111100000000000000000000000",
     +            b"01000000000000000000000000000000",
     +            b"01000000010000000000000000000000",
     +            b"01000000100000000000000000000000",
     +            b"01000000101000000000000000000000" /)
     +            + 0.0 /)
      RESULTr5 = assumshap1( 5,
     +arr_r5( (/ b"01",b"10",b"11",b"10",b"01" /) ) )
      if (
     +( .NOT. precision_r4( RESULTr5(1), 1.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr5(2), 2.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr5(3), 3.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr5(4), 2.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr5(5), 1.0 ) ) )
     +CALL ZZRC( CASENUM )
!
!- 2 Dimensional Assumed shape Array Dimensioning (Of real)
!- NOTE: here the 2 dimension result is reshaped into a
!- 1 Dimensional Array
!
      CASENUM = 21
      RESULTr9 = 0.0
      arr_r33(:,1) = (/ 1.0,2.0,3.0 /)
      arr_r33(:,2) = (/ arr_r33(:,1)+3.0 /)
      arr_r33(:,3) = (/ arr_r33(:,1)+arr_r33(:,2) /)
      RESULTr9 = RESHAPE((/assumshap2( 3,3,arr_r33 )/),(/9/))
      if (
     +( .NOT. precision_r4( RESULTr9(1), 1.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr9(2), 2.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr9(3), 3.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr9(4), 4.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr9(5), 5.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr9(6), 6.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr9(7), 5.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr9(8), 7.0 ) ) .OR.
     +( .NOT. precision_r4( RESULTr9(9), 9.0 ) ) )
     +CALL ZZRC(CASENUM)
C---------------------------------
C-
C- Assumed Size  Dimensions
C- (passing the Dimension As an
C-  actual Argument)
C-
C-----------------------------------

!
!- Dimension is an Integer Value
!
      CASENUM = 30
      RESULTi5 = 0
      arr_i5 = (/ 1,2,3,4,5 /)
      RESULTi5 = assumsize1( 5, arr_i5 )
      if (
     +( RESULTi5(1) .NE. 1 ) .OR.
     +( RESULTi5(2) .NE. 2 ) .OR.
     +( RESULTi5(3) .NE. 3 ) .OR.
     +( RESULTi5(4) .NE. 4 ) .OR.
     +( RESULTi5(5) .NE. 5 ) )
     +CALL ZZRC( CASENUM )
!
!- 2 Dimensional Array as Actual Argument
!
      CASENUM = 31
      RESULTi42 = 0
      arr_i42 = RESHAPE( (/ 1,2,3,4,5,6,7,8 /), (/ 4,2 /) )
      RESULTi42 = assumsize2( 4,2, arr_i42 )
      if (
     +( RESULTi42(1,1) .NE. 1 ) .OR.
     +( RESULTi42(2,1) .NE. 2 ) .OR.
     +( RESULTi42(3,1) .NE. 3 ) .OR.
     +( RESULTi42(4,1) .NE. 4 ) .OR.
     +( RESULTi42(1,2) .NE. 5 ) .OR.
     +( RESULTi42(2,2) .NE. 6 ) .OR.
     +( RESULTi42(3,2) .NE. 7 ) .OR.
     +( RESULTi42(4,2) .NE. 8 ) )
     +CALL ZZRC( CASENUM )
!
!- 2 Dimensional Array as Actual Argument But Assumed Size
!- receives it and returns it as 1 dimensional
!
      CASENUM = 32
      RESULTi8 = 0
      arr_i42 = RESHAPE( (/ 1,2,3,4,5,6,7,8 /), (/ 4,2 /) )
      RESULTi8 = (/ assumsize2( 4,2, arr_i42 ) /)
      if (
     +( RESULTi8(1) .NE. 1 ) .OR.
     +( RESULTi8(2) .NE. 2 ) .OR.
     +( RESULTi8(3) .NE. 3 ) .OR.
     +( RESULTi8(4) .NE. 4 ) .OR.
     +( RESULTi8(5) .NE. 5 ) .OR.
     +( RESULTi8(6) .NE. 6 ) .OR.
     +( RESULTi8(7) .NE. 7 ) .OR.
     +( RESULTi8(8) .NE. 8 ) )
     +CALL ZZRC( CASENUM )
C---------------------------------
C-
C- Deferred Shape Dimensions
C- (passing the Dimension As an
C-  actual Argument)
C-
C-----------------------------------

!
!- Dimension is an Integer Value
!
      CASENUM = 40
      RESULTch5 = '          '
      arr_ch5 = (/ 'Hello','hEllo','heLlo','helLo','hellO' /)
      RESULTch5 = deferred1( 5, arr_ch5//'     ' )
      if (
     +( RESULTch5(1) .NE. 'Hello     ' ) .OR.
     +( RESULTch5(2) .NE. 'hEllo     ' ) .OR.
     +( RESULTch5(3) .NE. 'heLlo     ' ) .OR.
     +( RESULTch5(4) .NE. 'helLo     ' ) .OR.
     +( RESULTch5(5) .NE. 'hellO     ' ) )
     +CALL ZZRC( CASENUM )

***********************************************************************


      CONTAINS

      FUNCTION explicit1( n, Arg1 )
         INTEGER n
         INTEGER, DIMENSION( n ) :: explicit1, Arg1
         INTEGER i
         do i = 1,n
             explicit1(i) = Arg1(i)
         enddo
         IF ( UBOUND(explicit1,1) .NE. n ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Arg1     ,1) .NE. n ) CALL ZZRC( CASENUM )
      END

      FUNCTION explicit1b( n, Arg1 )
         INTEGER n
         INTEGER, DIMENSION( -n:-1 ) :: explicit1b
         INTEGER, DIMENSION( n ) :: Arg1
         INTEGER i
         do i = 1,n
             explicit1b(i-n-1) = Arg1(i)
         enddo
         IF ( UBOUND(explicit1b,1) .NE. -1 ) CALL ZZRC( CASENUM )
         IF ( LBOUND(explicit1b,1) .NE. -n ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Arg1      ,1) .NE. n ) CALL ZZRC( CASENUM )
      END

      FUNCTION explicit2( m,n, Arg2 )
         INTEGER m,n
         INTEGER, DIMENSION(m,n) :: explicit2, Arg2
         INTEGER i,j
         IF ( UBOUND(explicit2,1) .NE. m ) CALL ZZRC( CASENUM )
         IF ( UBOUND(explicit2,2) .NE. n ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Arg2     ,1) .NE. m ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Arg2     ,2) .NE. n ) CALL ZZRC( CASENUM )
         do i = 1,m
            do j = 1,n
               explicit2(i,j) = Arg2(i,j)
            enddo
         enddo
      END

      FUNCTION automatic1( n, Arg1 )
         INTEGER n
         COMPLEX, DIMENSION( n ) :: automatic1, Arg1, Temp
         INTEGER i
         do i = 1,n
             Temp(i) = Arg1(i)
             automatic1(i) = Temp(i)
         enddo
         IF ( UBOUND(automatic1,1) .NE. n ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Arg1      ,1) .NE. n ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Temp      ,1) .NE. n ) CALL ZZRC( CASENUM )
      END

      FUNCTION automati2( m,n, Arg2 )
         INTEGER m,n
         COMPLEX, DIMENSION(m,n) :: automati2, Arg2, Temp
         INTEGER i,j
         IF ( UBOUND(automati2,1) .NE. m ) CALL ZZRC( CASENUM )
         IF ( UBOUND(automati2,2) .NE. n ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Arg2     ,1) .NE. m ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Arg2     ,2) .NE. n ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Temp     ,1) .NE. m ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Temp     ,2) .NE. n ) CALL ZZRC( CASENUM )
         do i = 1,m
            do j = 1,n
               Temp(i,j)      = Arg2(i,j)
               automati2(i,j) = Temp(i,j)
            enddo
         enddo
      END

      FUNCTION assumshap1( n, Arg1 )
         INTEGER n
         REAL   , DIMENSION( : ) :: Arg1
         REAL   , DIMENSION( SIZE(Arg1) ) :: assumshap1
         INTEGER i
         do i = 1,n
             assumshap1(i) = Arg1(i)
         enddo
         IF ( UBOUND(assumshap1,1) .NE. n ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Arg1     ,1) .NE. n ) CALL ZZRC( CASENUM )
      END

      FUNCTION assumshap2( m,n, Arg2 )
         INTEGER m,n
         REAL   , DIMENSION(:,:) :: Arg2
         REAL   , DIMENSION(m,n) :: assumshap2
         INTEGER i,j
         IF ( UBOUND(Arg2     ,1) .NE. m ) CALL ZZRC( CASENUM )
         IF ( UBOUND(Arg2     ,2) .NE. n ) CALL ZZRC( CASENUM )
         do i = 1,m
            do j = 1,n
               assumshap2(i,j) = Arg2(i,j)
            enddo
         enddo
      END

      FUNCTION assumsize1( n, Arg1 )
         INTEGER n
         INTEGER  , DIMENSION( * )  :: Arg1
         INTEGER  , DIMENSION( n )  :: assumsize1
         INTEGER i
         do i = 1,n
            assumsize1(i) = Arg1(i)
         enddo
      END

      FUNCTION assumsize2( m,n, Arg2 )
         INTEGER m,n
         INTEGER, DIMENSION(-m+1:0,*) :: Arg2
         INTEGER, DIMENSION(m,n) :: assumsize2
         INTEGER i,j
         do i = 1,m
            do j = 1,n
               assumsize2(i,j) = Arg2(i-m,j)
            enddo
         enddo
      END

      FUNCTION deferred1( n, Arg1a )
         INTEGER n
         character*10, DIMENSION( : ) :: Arg1a
         character*10, ALLOCATABLE, DIMENSION( : ) :: Arg1
         character*10, DIMENSION( n ) :: deferred1
         INTEGER i
         allocate( Arg1(n) )
         Arg1 = Arg1a
         do i = 1, n
            deferred1(i) = Arg1(i)
         enddo
      END

      FUNCTION deferred2( m,n, Arg1a )
         INTEGER m,n
         character*10, DIMENSION( :,: ) :: Arg1a
         character*10, ALLOCATABLE, DIMENSION( :,: ) :: Arg1
         character*10, DIMENSION(m,n) :: deferred2
         INTEGER i,j
         allocate( Arg1(m,n) )
         Arg1 = Arg1a
         do i = 1,m
            do j = 1,n
               deferred2(i,j) = Arg1(i,j)
            enddo
         enddo
      END

      END
