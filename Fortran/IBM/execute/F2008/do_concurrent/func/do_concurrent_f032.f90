!*********************************************************************
!  ===================================================================
!
!  TEST CASE NAME             : do_concurrent_f032.f
!
!  DATE                       : Sept 21, 2015
!
!  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT Construct
!  SECONDARY FUNCTIONS TESTED : PURE Function
!
!  DESCRIPTION                : Indirect array access inside a scalar-
!    mask-expr or in the DO CONCURRENT construct
!    Also covers some secondary tests:
!    - the scalar-mask-expression, concurrent-step or concurrent-limit could be:
!      - an expression
!      - an array element
!      - derived type component
!      - ...
!    - nesting
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

  integer, dimension(100) :: mod_index1 = (/ (j, j=1,100) /)
  integer, dimension(10)  :: mod_index2 = (/ (j, j=1,10) /)
  integer :: mod_limit1 = 100
  integer :: mod_limit2 = 10

  type base
    integer, dimension(100) :: index = (/ (i, i=1,100) /)
    integer :: limit1 = 100
  end type

  type (base) mod_base

  interface
    PURE MODULE LOGICAL FUNCTION submod_func1( l )
      integer, intent(in) :: l
    END FUNCTION

    module subroutine test5(l1a, r1b, r1c)
      integer, intent(in) :: l1a(100)
      real, intent(in) :: r1b(100,10)
      real, intent(inout) :: r1c(100,10)
    end subroutine
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

  module subroutine test5(l1a, r1b, r1c)
    integer, intent(in) :: l1a(100)
    real, intent(in) :: r1b(100,10)
    real, intent(inout) :: r1c(100,10)

    DO CONCURRENT ( integer :: in1 = 1:mod_index1(100), ( MODULO( l1a( mod_index1(in1)), 3 ) == 0) )
      DO CONCURRENT(integer :: in2 = 1:mod_base%limit1/mod_limit2, (r1b(in1,mod_index2(in2)) > 1))
        r1c(in1,in2) = 99
      END DO
      DO CONCURRENT(integer :: in2 = 1:mod_index2(10), .not.(r1b(mod_base%index(in1),in2) > 1))
        r1c(mod_base%index(in1),mod_index2(in2)) = -99
      END DO
    END DO
  end subroutine

END SUBMODULE subMod

PROGRAM do_concurrent_f029
   use iso_c_binding
   use mod
   implicit integer(j)

   logical, external :: precision_r4

   type, extends(base):: child
     integer, dimension(10) :: index2 = (/ (j, j=1,10) /)
     integer :: limit2 = 10
   end type

   integer, dimension(100) :: main_index1 = (/ (j, j=1,100) /)
   integer, dimension(10)  :: main_index2 = (/ (j, j=1,10) /)
   integer :: main_limit1 = 100
   integer :: main_limit2 = 10

   integer CASENUM
   integer in1,in2

   !--------------------------------
   ! counters
   !--------------------------------

   type (base) base1
   type (child) child1

   !--------------------------------
   ! Variable Declarations
   !--------------------------------

   INTEGER l1a(100)
   INTEGER i1a(100)
   INTEGER i1b(100,10)

   REAL r1a(100,10)
   REAL r1b(100,10)
   REAL r1c(100,10)

   COMPLEX c1a(100,10)
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
   !  indirect array access related to  pure function in scalar-mask-expression / loop body
   !  - the indirect array is a derived type component
   !  - concurrent-limit is a derived type integer array element
   !-------------------------------
   CASENUM = 1
   PRINT *,CASENUM

   i1a = 0
   DO CONCURRENT ( in1 = 1:base1%index(100), func1(l1a(base1%index(in1))) )
      i1a(base1%index(in1)) = in1
   END DO

   print *,i1a

   !-------------------------------
   !  indirect array access in external (module and submodule) pure function in a scalar-mask-expression of a nested DO CONCURRENT construct and loop body
   !  - the indirect array is a derived type component/child of
   !  - concurrent-limit is an expression involving integer, and derived type integer component
   !-------------------------------
   CASENUM = 2
   PRINT *,CASENUM

   i1b = 0
   DO CONCURRENT ( in1 = 1:main_limit2*child1%limit2, mod_func1(l1a(child1%index(in1))) )
      DO CONCURRENT ( in2 = 1:mod_limit2, submod_func1(l1a(child1%index2(in2))) )
         i1b(child1%index(in1),child1%index2(in2)) = in1 + in2
      END DO
   END DO

   print *,i1b

   !-------------------------------
   !  indirect array access in external pure function in DO CONCURRENT scalar-mask-expr
   !  - the indirect array is in the program scope, module scope
   !  - complex part is used in the loop body
   !  - concurrent-limit is an array element, concurrent-step is a function result
   !  - nested case concurrent-step is an integer array element
   !  - nested case concurrent-limit is a function result, concurrent-limit is an derived-type array element
   !-------------------------------
   CASENUM = 3
   PRINT *,CASENUM

   r1a = 0.0
   DO CONCURRENT ( in1 = mod_index1(1):100:modulo(3,2), submod_func1(main_index1(l1a(in1))) )
      DO CONCURRENT ( in2 = 1:10:child1%index(2) )
        c1a(in1,in2) = (0, in1+in2)
      END DO
   END DO

   DO CONCURRENT ( in1 = 1:100, submod_func1(mod_index1(l1a(in1))) )
      DO CONCURRENT ( in2 = modulo(3,2):10:child1%index(2) )
          r1a(mod_index1(in1),in2) = imag(c1a(in1,mod_index2(in2)))
      END DO
   END DO

   DO in1 = 1,100, 3
     DO in2 = 1,10,2
        if( .not.precision_r4( real(c1a(in1,in2)),0)) then
          print *, "realpart(c1a(in1,in2) != 0, in1=", in1, ",in2=",in2,",c1a(in1,in2)=",c1a(in1,in2)
          error stop 3
        end if
      END DO
   END DO
   print *,r1a

   !-------------------------------
   !  nested indirect array access in external pure function (from c program) in nested DO CONCURRENT scalar-mask-expr/loop body
   !  - the indirect array is in the program scope
   !  - acting on an array section
   !-------------------------------
   CASENUM = 4
   PRINT *,CASENUM

   r1b(:,::2)  = 1.0
   r1b(:,2::2) = 2.0
   DO CONCURRENT ( in1 = 1:100, mod_func1(l1a(main_index1(in1))) )
     DO CONCURRENT ( in2 = 1:10, c_greaterthan1(r1b(in1,main_index2(in2))) )
       r1b(main_index1(in1),in2) = 99.0
     END DO
   END DO

   print *,r1b

   !-------------------------------
   !  indirect array access in an external subroutine nested DO CONCURRENT scalar-mask-expr
   !  - acting on an array section
   !-------------------------------
   CASENUM = 5
   PRINT *,CASENUM

   r1b(:,::2)  = 1.0
   r1b(:,2::2) = 2.0
   r1c = 0.0

   call test5(l1a, r1b, r1c)

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
