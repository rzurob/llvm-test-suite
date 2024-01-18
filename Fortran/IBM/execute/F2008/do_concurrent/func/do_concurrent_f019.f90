!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : do_concurrent_f019.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2015-02-26
!*  ORIGIN                     :
!*  FEATURE                    : F2008 DO CONCURRENT
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                : - nesting various loops in DO CONCURRENT
!*
!*  - based on hpf_forall/construct/fxfc032b.f
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM fxfc032b

   INTEGER CASENUM,i,j,k

   interface
      SUBROUTINE sub1( n )
         INTEGER n
      END SUBROUTINE
      
      SUBROUTINE sub2( n )
         INTEGER n
      END SUBROUTINE
        
      SUBROUTINE sub3( n )
         INTEGER n
      END SUBROUTINE 
      
      SUBROUTINE sub4( m,n )
         INTEGER m,n
      END SUBROUTINE
      
      SUBROUTINE sub4b( m,n )
         INTEGER m,n
      END SUBROUTINE
      
      SUBROUTINE sub5( m,n )
         INTEGER m,n
      END SUBROUTINE
      
      SUBROUTINE sub6( m,n )
         INTEGER m,n
      END SUBROUTINE
      
      SUBROUTINE sub7( m,n )
         INTEGER m,n
      END SUBROUTINE
      
      SUBROUTINE sub8( m,n )
         INTEGER m,n
      END SUBROUTINE
      
      SUBROUTINE sub9( m,n )
         INTEGER m,n
      END SUBROUTINE
      
      SUBROUTINE sub10( m,n )
         INTEGER m,n
      END SUBROUTINE
   end interface

!==================================================================

   CASENUM = 1
   PRINT *,"CASENUM=",CASENUM
   CALL sub1( 100 )

   CASENUM = 2
   PRINT *,"CASENUM=",CASENUM
   i = 100
   CALL sub2( i )

   CASENUM = 3
   PRINT *,"CASENUM=",CASENUM
   CALL sub3( 100 )

   CASENUM = 4
   PRINT *,"CASENUM=",CASENUM
   CALL sub4( 20,20 )

   CASENUM = 5
   PRINT *,"CASENUM=",CASENUM
   CALL sub5( 20,20 )

   CASENUM = 6
   PRINT *,"CASENUM=",CASENUM
   i = 20
   j = 20
   CALL sub6( i,j )

   CASENUM = 7
   PRINT *,"CASENUM=",CASENUM
   CALL sub7( 20,20 )

   CASENUM = 8
   PRINT *,"CASENUM=",CASENUM
   CALL sub8( 20,20 )

   CASENUM = 9
   PRINT *,"CASENUM=",CASENUM
   CALL sub9( 20,20 )

   CASENUM = 10
   PRINT *,"CASENUM=",CASENUM
   CALL sub9( 20,20 )

END PROGRAM

!==================================================================

!-------------------------------
! This subroutine accepts a one
! dimensional array and assigns
! incrementing values to it.
!
! Test: assigment-stmt
!
!-------------------------------

SUBROUTINE sub1( n )
   INTEGER n,array(n)
   INTEGER in1

   array = 0
   DO CONCURRENT ( in1 = 1:ubound(array,1) )
      array(in1) = in1
   END DO
   PRINT *,array
END SUBROUTINE

!-------------------------------
! This subroutine accepts a one
! dimensional array and assigns
! values to it.
!
! Test: FORALL nested in DO CONCURRENT
!
!-------------------------------

SUBROUTINE sub2( n )
   INTEGER n,array(-10:n)
   INTEGER in1,in2

   array = 0
   FORALL ( in1 = -9:ubound(array,1) )
      array(in1-1) = in1
      FORALL (in2 = in1:in1)
        array(in2) = array(in2-1)
      END FORALL
   END FORALL
   PRINT *,array
END SUBROUTINE
  

!-------------------------------
! This subroutine accepts a one
! dimensional array and assigns
! values to it.
! 
! Test: DO nested in DO CONCURRENT
!
!-------------------------------

SUBROUTINE sub3( n )
   INTEGER n,array(n)
   INTEGER in1,in2

   array = 0
   DO CONCURRENT ( in1 = 4:ubound(array,1):4 )
      DO in2 = 3,0,-2 
         array(in1-in2:in1-in2+1) = in1+in2
      END DO
   END DO
   PRINT *,array
END SUBROUTINE 

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: assignment-stmt
!
!-------------------------------

SUBROUTINE sub4( m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1,in2

   array = 0
   DO CONCURRENT ( in1 = 1:ubound(array,1),in2=1:ubound(array,2) )
      array(in1,in2) = in1+in2
   END DO
   PRINT *,array
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: array assignment-stmt
!
!-------------------------------

SUBROUTINE sub4b( m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1

   array = 0
   DO CONCURRENT ( in1 = 1:ubound(array,1) )
      array(in1,:) = in1
   END DO
   PRINT *,array
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: simple scalar-mask-expr in DO CONCURRENT
!
!-------------------------------

SUBROUTINE sub5( m,n )
   INTEGER m,n,array(-5:m,-2:n)
   INTEGER in1,in2

   array = 0
   forall (in1=-5:ubound(array,1),in2=-2:ubound(array,2)) &
      array(in1,in2) = in1+in2

   DO CONCURRENT ( in1=-5:ubound(array,1), in2=-2:ubound(array,2), array(in1,in2) .GT. 0 )
      array(in1,in2) = -99
   END DO
   PRINT *,array
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: FORALL with WHERE-construct nested in DO CONCURRENT
!
!-------------------------------

SUBROUTINE sub6( m,n )
   INTEGER m,n,array(-5:m,-2:n)
   INTEGER in1,in2

   array = 0
   DO CONCURRENT ( in1=-5:ubound(array,1) )
      forall (in2=-2:ubound(array,2) ) array(in1,in2) = in1 + in2
      WHERE ( array(in1,::2) .GT. 0 )
         array(in1,::2) = -99
         array(in1,n:-2:-2) = -98
      END WHERE
   END DO
   PRINT *,array
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: DO, DO CONCURRENT and FORALL nested in a  DO CONCURRENT
!
!-------------------------------

SUBROUTINE sub7( m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1,in2

   array = 0
   DO CONCURRENT ( in1 = 1:ubound(array,1) )
      DO in2 = 1,ubound(array,2) 
        array(in1,in2) = in1 + in2
      END DO
      DO CONCURRENT ( in2 = 1:ubound(array,2),in1 .EQ. in2)
        array(in1,in2) = -99
      END DO
      forall ( in2 = 2:ubound(array,2):2 ) array(in1,in2) = array(in1,in2-1)
   END DO
   PRINT *,array
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: array forall-stmt nested in DO CONCURRENT
!
!-------------------------------

SUBROUTINE sub8( m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1,in2

   array = 0
   DO CONCURRENT ( in1 = 1:ubound(array,1) )
      forall ( in2 = 3:ubound(array,2):3 ) &
         array(in1,in2-2:in2) = (/ in1,in2,in1+in2 /)
   END DO
   PRINT *,array
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: FORALL and DO CONCURRENT nested in DO CONCURRENT
!
!-------------------------------

SUBROUTINE sub9( m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1,in2

   array = 0
   DO CONCURRENT ( in1 = 1:ubound(array,1) )
      FORALL ( in2 = 2:ubound(array,2):2 )
         array(in1,in2-1) = in1 + in2 -1
         array(in1,in2)   = in1 + in2
      END FORALL
      DO CONCURRENT ( in2 = 1:ubound(array,2), in1 .EQ. in2 )
         array(in1,in2) = -99
      END DO
   END DO
   PRINT *,array
END SUBROUTINE

!-------------------------------
! This subroutine accepts a two
! dimensional array and assigns
! values to it.
!
! Test: array FORALL nested in DO CONCURRENT
!
!-------------------------------

SUBROUTINE sub10( m,n )
   INTEGER m,n,array(m,n)
   INTEGER in1,in2

   array = 0
   DO CONCURRENT( in1 = 1:ubound(array,1) )
      FORALL ( in2 = 1:ubound(array,2):ubound(array,2) )
         array(in1,in2) = in1
         array(in1,in2+1::2) = array(in1,in2:n-1:2)
         array(in1,in2) = array(in1,in2) + 1
      END FORALL
   END DO
   PRINT *,array
END SUBROUTINE


