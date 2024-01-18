!*********************************************************************
!  ===================================================================
!
!  DATE                       : Sept 21, 2015
!
!  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT Construct
!  SECONDARY FUNCTIONS TESTED : PURE Function
!
!  DESCRIPTION                : CYCLE in a DO CONCURRENT, verify using a
!                               scalar-mask-expr function that skips
!                               iterations
!
!  KEYWORD(S)                 :
!  TARGET(S)                  :
!  NUMBER OF TESTS            : ??
!  STATUS                     :
!
!  STRUCTURE                  : Main program
!  EXECUTABLE                 : Yes
!
!  INPUTS                     : None
!  OUTPUTS                    : None
!
!  SETUP REQUIREMENTS         : N/A
!  DEPENDENCIES               : External routine ZZRC
!
!  REQUIRED COMPILER OPTIONS  : None
!
!  NORMAL COMPLETION          : Return code = 0
!  ABNORMAL COMPLETION        : Return code ^= 0
!
!  RUN TIME ESTIMATE          : <60 SECS
!
!  ASSUMPTIONS                : None
!
!  CONDITIONS TESTED          :
!
!  - based on /tstdev/hpf_forall/construct/fxfc015a.f
! ===================================================================
!**********************************************************************

PROGRAM do_concurrent_f030
   implicit none

   integer CASENUM
   integer in1,in2

   !--------------------------------
   ! Variable Declarations
   !--------------------------------

   INTEGER l1a(100), cycle_l1a(100)
   INTEGER i1a(100), cycle_i1a(100)
   INTEGER i1b(100,10), cycle_i1b(100,10)

   REAL r1a(100,10), cycle_r1a(100,10)
   REAL r1b(100,10), cycle_r1b(100,10)
   REAL r1c(100,10), cycle_r1c(100,10)

   !--------------------------------
   ! End of Variable Declarations
   !--------------------------------

   !--------------------------------
   ! Interface Block
   !--------------------------------

   INTERFACE

      PURE LOGICAL FUNCTION func1( l )
         INTEGER,INTENT(IN) :: l
      END FUNCTION

   END INTERFACE

   !--------------------------------
   ! End of Interface Block
   !--------------------------------

   l1a = (/ (in1,in1=1,100) /)

   !-------------------------------
   CASENUM = 1
   PRINT *,CASENUM

   i1a = 0
   cycle_i1a = 0
   DO CONCURRENT ( in1 = 1:100, func1(l1a(in1)) )
      i1a(in1) = in1
   END DO

   ! CYCLE equivalent
   DO CONCURRENT ( in1 = 1:100)
      if ( MODULO(in1,3) .ne. 0) cycle
      cycle_i1a(in1) = in1
   END DO

   if (any(i1a .ne. cycle_i1a)) error stop 1

   !-------------------------------
   CASENUM = 2
   PRINT *,CASENUM

   i1b = 0
   cycle_i1b = 0
   DO CONCURRENT ( in1 = 1:100, func1(l1a(in1)) )
      DO CONCURRENT ( in2 = 1:10, func1(l1a(in2)) )
         i1b(in1,in2) = in1 + in2
      END DO
   END DO

   ! CYCLE equivalent
   DO CONCURRENT ( in1 = 1:100 )
      if ( MODULO(in1,3) .ne. 0) cycle
      DO CONCURRENT ( in2 = 1:10 )
         if ( MODULO(in2,3) .ne. 0) cycle
         cycle_i1b(in1,in2) = in1 + in2
      END DO
   END DO

   if (any(i1b .ne. cycle_i1b)) error stop 2

   !-------------------------------
   !  external pure function in DO CONCURRENT scalar-mask-expr
   !-------------------------------
   CASENUM = 3
   PRINT *,CASENUM

   r1a = 0.0
   cycle_r1a = 0.0
   DO CONCURRENT ( in1 = 1:100, func1(l1a(in1)) )
      DO CONCURRENT ( in2 = 1:10:2 )
        r1a(in1,in2) = real(in1+in2)
      END DO
   END DO

   ! CYCLE equivalent
   DO CONCURRENT ( in1 = 1:100 )
      if ( MODULO(in1,3) .ne. 0) cycle
      DO CONCURRENT ( in2 = 1:10:2 )
        cycle_r1a(in1,in2) = real(in1+in2)
      END DO
   END DO

   if (any(r1a .ne. cycle_r1a)) error stop 3

   !-------------------------------
   !  external pure function (from c program) in nested DO CONCURRENT scalar-mask-expr
   !-------------------------------
   CASENUM = 4
   PRINT *,CASENUM

   r1b(:,::2)  = 1.0
   cycle_r1b(:,::2)  = 1.0
   r1b(:,2::2) = 2.0
   cycle_r1b(:,2::2) = 2.0
   DO CONCURRENT ( in1 = 1:100, func1(l1a(in1)) )
     DO CONCURRENT ( in2 = 1:10, greaterthan1(r1b(in1,in2)))
       r1b(in1,in2) = 99.0
     END DO
   END DO

   ! CYCLE equivalent
   DO CONCURRENT ( in1 = 1:100 )
     if ( MODULO(in1,3) .ne. 0) cycle
     DO CONCURRENT ( in2 = 1:10, greaterthan1(r1b(in1,in2)))
       cycle_r1b(in1,in2) = 99.0
     END DO
   END DO

   if (any(r1b .ne. cycle_r1b)) error stop 4

   !-------------------------------
   !  external pure function in nested DO CONCURRENT scalar-mask-expr
   !-------------------------------
   CASENUM = 5
   PRINT *,CASENUM

   r1b(:,::2)  = 1.0
   cycle_r1b(:,::2)  = 1.0
   r1b(:,2::2) = 2.0
   cycle_r1b(:,2::2) = 2.0
   r1c = 0.0
   cycle_r1c = 0.0

   DO CONCURRENT ( in1 = 1:100, func1(l1a(in1)) )
     DO CONCURRENT(in2 = 1:10, greaterThan1(r1b(in1,in2)))
       r1c(in1,in2) = 99
     END DO
     DO CONCURRENT(in2 = 1:10, .not. greaterThan1(r1b(in1,in2)))
       r1c(in1,in2) = -99
     END DO
   END DO

   ! CYCLE equivalent
   DO CONCURRENT ( in1 = 1:100 )
     if ( MODULO(in1,3) .ne. 0) cycle
     DO CONCURRENT(in2 = 1:10, greaterThan1(cycle_r1b(in1,in2)))
       cycle_r1c(in1,in2) = 99
     END DO
     DO CONCURRENT(in2 = 1:10, .not. greaterThan1(cycle_r1b(in1,in2)))
       cycle_r1c(in1,in2) = -99
     END DO
   END DO

   if (any(r1c .ne. cycle_r1c)) error stop 5

   !-------------------------------
   CASENUM = 7
   PRINT *,CASENUM

   i1a = 0
   cycle_i1a = 0
   i1b = 0
   r1a = 0.0
   r1b(:,::2) = 1.0
   r1b(:,2::2) = 2.0
   r1c = 0.0

   DO CONCURRENT( in1 = 1:100, func1(l1a(in1)) )
     i1a(in1) = in1
   END DO

   ! CYCLE equivalent
   DO CONCURRENT( in1 = 1:100 )
     if ( MODULO(in1,3) .ne. 0) cycle
     cycle_i1a(in1) = in1
   END DO

   DO CONCURRENT( in1 = 1:100, func1(l1a(in1)) )
      FORALL ( in2 = 1:10, func1(l1a(in2)) )
         i1b(in1,in2) = in1 + in2
      END FORALL

      DO CONCURRENT (in2=1:10, greaterThan1(r1b(in1,in2)))
         r1b(in1,in2) = 99
      END DO

      forall ( in2=1:10:2 ) r1a(in1,in2) = real(in1+in2)

      DO CONCURRENT(in2=1:10, greaterThan1(r1b(in1,in2)))
         r1c(in1,in2) = 99
      END DO

      DO CONCURRENT(in2=1:10, .not. greaterthan1(r1b(in1,in2)))
         r1c(in1,in2) = -99
      END DO

   END DO

   ! CYCLE equivalent
   DO CONCURRENT( in1 = 1:100 )
     if ( MODULO(in1,3) .ne. 0) cycle
      FORALL ( in2 = 1:10, func1(l1a(in2)) )
         cycle_i1b(in1,in2) = in1 + in2
      END FORALL

      DO CONCURRENT (in2=1:10, greaterThan1(r1b(in1,in2)))
         cycle_r1b(in1,in2) = 99
      END DO

      forall ( in2=1:10:2 ) r1a(in1,in2) = real(in1+in2)

      DO CONCURRENT(in2=1:10, greaterThan1(r1b(in1,in2)))
         cycle_r1c(in1,in2) = 99
      END DO

      DO CONCURRENT(in2=1:10, .not. greaterthan1(r1b(in1,in2)))
         cycle_r1c(in1,in2) = -99
      END DO

   END DO

   if (any(i1a .ne. cycle_i1a)) error stop 6
   if (any(i1b .ne. cycle_i1b)) error stop 7
   if (any(r1a .ne. cycle_r1a)) error stop 8
   if (any(r1b .ne. cycle_r1b)) error stop 9
   if (any(r1c .ne. cycle_r1c)) error stop 10

CONTAINS
  PURE LOGICAL FUNCTION greaterThan1(r)
    real, intent(in) :: r
    if (r > 1.0) then
      greaterThan1 = .true.
    else
      greaterThan1 = .false.
    end if
  END FUNCTION
END

!----------------------------------------------------

PURE LOGICAL FUNCTION func1( l )
   INTEGER,INTENT(IN) :: l

   if ( MODULO(l,3) == 0 ) THEN
      func1 = .TRUE.
   else
      func1 = .FALSE.
   end if

END FUNCTION

!----------------------------------------------------
