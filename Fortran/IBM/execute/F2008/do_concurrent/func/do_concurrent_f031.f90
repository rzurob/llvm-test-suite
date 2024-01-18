!*********************************************************************
!  ===================================================================
!
!  TEST CASE NAME             : do_concurrent_f031.f
!
!  DATE                       : Sept 21, 2015
!
!  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT Construct
!  SECONDARY FUNCTIONS TESTED : PURE Function
!
!  DESCRIPTION                :
!   Use implicitly declared variables as indeces in a DO CONCURRENT construct.
!   The varlable could be specified with the implicit keyword or not specified
!   Also covers the use of DO CONCURRENT in various scopes
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
  implicit INTEGER(x-z)
  implicit INTEGER*2(u-w)

  interface
    PURE MODULE LOGICAL FUNCTION submod_func1( l )
      integer, intent(in) :: l
    END FUNCTION

    MODULE SUBROUTINE test3( r1a, l1a )
      REAL, intent(inout) :: r1a(100,10)
      INTEGER, intent(in) :: l1a(100)
    END SUBROUTINE

    MODULE LOGICAL FUNCTION test2( i1b, l1a )
      INTEGER, intent(inout) :: i1b(100,10)
      INTEGER, intent(in) :: l1a(100)
    END FUNCTION

    MODULE SUBROUTINE test7 ( r1a, r1b, r1c, l1a, i1a, i1b )
      REAL, intent(inout) :: r1a(100,10), r1b(100,10), r1c(100,10)
      INTEGER, intent(inout) :: i1a(100), i1b(100,10)
      INTEGER, intent(in) :: l1a(100)
    END SUBROUTINE
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

  ! use DO CONCURRENT inside a module subroutine
  MODULE SUBROUTINE test3( r1a, l1a )
    REAL, intent(inout) :: r1a(100,10)
    INTEGER, intent(in) :: l1a(100)

    DO CONCURRENT ( x = 1:100, submod_func1(l1a(x)) )
      DO CONCURRENT ( y = 1:10:2 )
        r1a(x,y) = real(x+y)
      END DO
    END DO
  END SUBROUTINE

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

  MODULE LOGICAL FUNCTION test2( i1b, l1a )
    INTEGER, intent(inout) :: i1b(100,10)
    INTEGER, intent(in) :: l1a(100)

    test2 = .false.
    DO CONCURRENT ( i = 1:100, mod_func1(l1a(i)) )
      DO CONCURRENT ( j = 1:10, submod_func1(l1a(j)) )
         i1b(i,j) = i + j
      END DO
    END DO
    test2 = .true.
  END FUNCTION

  MODULE PROCEDURE test7
    implicit INTEGER(x-z)
    DO CONCURRENT( x = 1:100, mod_func1(l1a(x)) )
       i1a(x) = x
    END DO

    DO CONCURRENT( x = 1:100, mod_func1(l1a(x)) )
       FORALL ( y = 1:10, submod_func1(l1a(y)) )
          i1b(x,y) = x + y
       END FORALL

       DO CONCURRENT (y=1:10, (r1b(x,y) > 1))
          r1b(x,y) = 99
       END DO

       forall ( y=1:10:2 ) r1a(x,y) = real(x+y)

       DO CONCURRENT(y=1:10, (r1b(x,y) > 1))
          r1c(x,y) = 99
       END DO

       DO CONCURRENT(y=1:10, .not. (r1b(x,y) > 1))
          r1c(x,y) = -99
       END DO

    END DO
  END PROCEDURE

END SUBMODULE subMod

PROGRAM do_concurrent_f031
   use mod
   implicit INTEGER*2(u-w)
   implicit INTEGER(x-z)

   integer CASENUM

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

      LOGICAL FUNCTION test4( r1b, l1a )
        REAL, intent(inout) :: r1b(100,10)
        INTEGER, intent(in) :: l1a(100)
      END FUNCTION

      SUBROUTINE test5( r1b, r1c, l1a )
         REAL, intent(inout) :: r1b(100:10)
         REAL, intent(inout) :: r1c(100:10)
         INTEGER, intent(in) :: l1a(100)
      END SUBROUTINE
   END INTERFACE

   !--------------------------------
   ! End of Interface Block
   !--------------------------------

   l1a = (/ (x,x=1,100) /)

   !-------------------------------
   !  in program scope
   !-------------------------------
   CASENUM = 1
   PRINT *,CASENUM

   i1a = 0
   DO CONCURRENT ( x = 1:100, func1(l1a(x)) )
      i1a(x) = x
   END DO

   print *,i1a

   !-------------------------------
   !  in an external module function scope
   !-------------------------------
   CASENUM = 2
   PRINT *,CASENUM

   i1b = 0

   if (.not. test2( i1b, l1a )) error stop 2

   print *,i1b

   !-------------------------------
   !  in an external module subroutine scope
   !-------------------------------
   CASENUM = 3
   PRINT *,CASENUM

   r1a = 0.0

   call test3( r1a, l1a )

   print *,r1a

   !-------------------------------
   !  in an external function scope
   !-------------------------------
   CASENUM = 4
   PRINT *,CASENUM

   r1b(:,::2)  = 1.0
   r1b(:,2::2) = 2.0

   if (.not. test4( r1b, l1a )) error stop 4

   print *,r1b

   !-------------------------------
   !  in an external subroutine scope
   !-------------------------------
   CASENUM = 5
   PRINT *,CASENUM

   r1b(:,::2)  = 1.0
   r1b(:,2::2) = 2.0
   r1c = 0.0

   call test5( r1b, r1c, l1a)

   print *,r1c

   !-------------------------------
   !  in a module subprogram scope
   !-------------------------------
   CASENUM = 7
   PRINT *,CASENUM

   i1a = 0
   i1b = 0
   r1a = 0.0
   r1b(:,::2) = 1.0
   r1b(:,2::2) = 2.0
   r1c = 0.0

   call test7( r1a, r1b, r1c, l1a, i1a, i1b )
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

LOGICAL FUNCTION test4( r1b, l1a )
  implicit INTEGER(x-z)
  REAL, intent(inout) :: r1b(100,10)
  INTEGER, intent(in) :: l1a(100)

  test4 = .false.
  DO CONCURRENT ( x = 1:100, modulo(l1a(x),3)==0 )
    DO CONCURRENT ( y = 1:10, (r1b(x,y) > 1))
      r1b(x,y) = 99.0
    END DO
  END DO
  test4 = .true.
END FUNCTION

SUBROUTINE test5( r1b, r1c, l1a )
   REAL, intent(inout) :: r1b(100,10)
   REAL, intent(inout) :: r1c(100,10)
   INTEGER, intent(in) :: l1a(100)

   DO CONCURRENT ( i = 1:100, modulo(l1a(i),3)==0 )
     DO CONCURRENT(j = 1:10, (r1b(i,j) > 1))
       r1c(i,j) = 99
     END DO
     DO CONCURRENT(j = 1:10, .not. (r1b(i,j)>1))
       r1c(i,j) = -99
     END DO
   END DO
END SUBROUTINE

!----------------------------------------------------
