!*********************************************************************
!  ===================================================================
!  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!  ===================================================================
!
!  TEST CASE NAME             : do_concurrent_f029.f
!  TEST CASE TITLE            :
!
!  PROGRAMMER                 : Bernard Kan
!  DATE                       : Sept 21, 2015
!  ORIGIN                     : AIX Compiler Development, Toronto Lab
!
!  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT Construct
!  SECONDARY FUNCTIONS TESTED : PURE Function
!
!  DESCRIPTION                : Test a PURE function as the scalar-
!                               mask-expr in a DO CONCURRENT construct.
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

MODULE mod
  interface
    PURE MODULE LOGICAL FUNCTION submod_func1( l )
      integer, intent(in) :: l
    END FUNCTION
  end interface

contains
  PURE LOGICAL FUNCTION mod_func1( l )
    INTEGER,INTENT(IN) :: l

    if ( MODULO(l,3) == 0 ) THEN
      mod_func1 = .TRUE.
    else
      mod_func1 = .FALSE.
    end if
  END FUNCTION

END MODULE mod

SUBMODULE (mod) subMod
contains
  MODULE PURE LOGICAL FUNCTION submod_func1( l )
    INTEGER,INTENT(IN) :: l

    if ( MODULO(l,3) == 0 ) THEN
      submod_func1 = .TRUE.
    else
      submod_func1 = .FALSE.
    end if

  END FUNCTION
END SUBMODULE subMod

PROGRAM do_concurrent_f029
   use iso_c_binding
   use mod
   implicit none

   integer CASENUM
   integer in1,in2

   !--------------------------------
   ! Variable Declarations
   !--------------------------------

   INTEGER l1a(100)
   INTEGER i1a(100)
   INTEGER i1b(100,10)

   REAL r1a(100,10)
   REAL r1b(100,10)
   REAL r1c(100,10)

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

      PURE LOGICAL(C_BOOL) FUNCTION c_greaterthan1(r)
         import
         REAL(C_FLOAT), intent(in) :: r
      END FUNCTION
   END INTERFACE

   !--------------------------------
   ! End of Interface Block
   !--------------------------------

   l1a = (/ (in1,in1=1,100) /)

   !-------------------------------
   !  pure function in scalar-mask-expression
   !-------------------------------
   CASENUM = 1
   PRINT *,CASENUM

   i1a = 0
   DO CONCURRENT ( in1 = 1:100, func1(l1a(in1)) )
      i1a(in1) = in1
   END DO

   print *,i1a

   !-------------------------------
   !  external (module and submodule) pure function in a scalar-mask-expression of a nested DO CONCURRENT construct
   !-------------------------------
   CASENUM = 2
   PRINT *,CASENUM

   i1b = 0
   DO CONCURRENT ( in1 = 1:100, mod_func1(l1a(in1)) )
      DO CONCURRENT ( in2 = 1:10, submod_func1(l1a(in2)) )
         i1b(in1,in2) = in1 + in2
      END DO
   END DO

   print *,i1b

   !-------------------------------
   !  external pure function in DO CONCURRENT scalar-mask-expr
   !-------------------------------
   CASENUM = 3
   PRINT *,CASENUM

   r1a = 0.0
   DO CONCURRENT ( in1 = 1:100, submod_func1(l1a(in1)) )
      DO CONCURRENT ( in2 = 1:10:2 )
        r1a(in1,in2) = real(in1+in2)
      END DO
   END DO

   print *,r1a

   !-------------------------------
   !  external pure function (from c program) in nested DO CONCURRENT scalar-mask-expr
   !-------------------------------
   CASENUM = 4
   PRINT *,CASENUM

   r1b(:,::2)  = 1.0
   r1b(:,2::2) = 2.0
   DO CONCURRENT ( in1 = 1:100, mod_func1(l1a(in1)) )
     DO CONCURRENT ( in2 = 1:10, c_greaterthan1(r1b(in1,in2)))
       r1b(in1,in2) = 99.0
     END DO
   END DO

   print *,r1b

   !-------------------------------
   !  external pure function in nested DO CONCURRENT scalar-mask-expr
   !-------------------------------
   CASENUM = 5
   PRINT *,CASENUM

   r1b(:,::2)  = 1.0
   r1b(:,2::2) = 2.0
   r1c = 0.0
   DO CONCURRENT ( in1 = 1:100, submod_func1(l1a(in1)) )
     DO CONCURRENT(in2 = 1:10, c_greaterThan1(r1b(in1,in2)))
       r1c(in1,in2) = 99
     END DO
     DO CONCURRENT(in2 = 1:10, .not. greaterThan1(r1b(in1,in2)))
       r1c(in1,in2) = -99
     END DO
   END DO

   print *,r1c

   !-------------------------------
   CASENUM = 7
   PRINT *,CASENUM

   i1a = 0
   i1b = 0
   r1a = 0.0
   r1b(:,::2) = 1.0
   r1b(:,2::2) = 2.0
   r1c = 0.0

   DO CONCURRENT( in1 = 1:100, func1(l1a(in1)) )
      i1a(in1) = in1
   END DO

   DO CONCURRENT( in1 = 1:100, mod_func1(l1a(in1)) )
      FORALL ( in2 = 1:10, submod_func1(l1a(in2)) )
         i1b(in1,in2) = in1 + in2
      END FORALL

      DO CONCURRENT (in2=1:10, greaterThan1(r1b(in1,in2)))
         r1b(in1,in2) = 99
      END DO

      forall ( in2=1:10:2 ) r1a(in1,in2) = real(in1+in2)

      DO CONCURRENT(in2=1:10, c_greaterThan1(r1b(in1,in2)))
         r1c(in1,in2) = 99
      END DO

      DO CONCURRENT(in2=1:10, .not. greaterthan1(r1b(in1,in2)))
         r1c(in1,in2) = -99
      END DO

   END DO

   print *,i1a
   print *,i1b
   print *,r1a
   print *,r1b
   print *,r1c

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
