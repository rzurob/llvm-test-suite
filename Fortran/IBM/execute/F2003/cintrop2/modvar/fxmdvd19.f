!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/rundev.sh fxmdvd19 cxmdvd19a cxmdvd19b cxmdvd19c
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov 20, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   : Test derived types (up to three levels nested)
!*                               integer variables, the combination of type
!*                               and kind type  parameter,
!*                               with bind(c) attribute/statement,
!*                               is interoperate with  corresponding C type.
!*
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : C_INT, C_INT_FAST64_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_INT and C_INT_FAST64_T
!*	- Use multiple C code files in the testcases.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module derivemod
  use ISO_C_BINDING

  type, bind(c) :: dt0
     integer(C_INT) :: a
     integer(C_INT_FAST64_T) :: b
  end type dt0

  type, bind(c) :: dt1
     type(dt0) :: d0
     integer(C_INT) :: a
     integer(C_INT_FAST64_T) :: b
  end type dt1

  type, bind(c) :: dt2
     type(dt1) :: d1
     integer(C_INT) :: a
     integer(C_INT_FAST64_T) :: b
  end type dt2

  type(dt0) :: dta

  type(dt1) :: dtb

  type(dt2) :: dtc

end module derivemod

program p1
  use derivemod

  interface

     subroutine csub1(x)
       use derivemod
       type(dt0) :: x
     end subroutine csub1

     subroutine csub2(x)
       use derivemod
       type(dt1) :: x
     end subroutine csub2

     subroutine csub3(x)
       use derivemod
       type(dt2) :: x
     end subroutine csub3

  end interface

  !Testcase 1 : 1 level derived type
  type(dt0) :: y
  logical::res
  dta = dt0(5 , 10)
  print *, 'dta%a =', dta%a
  print *, 'dta%b =', dta%b


  y = dt0(2, 4)
  call csub1(dta)
  res= ((dta%a == y%a).AND. (dta%b == y%b) )
  if ( res .eqv. .FALSE.) then
     error stop 220
  endif

  print *, 'dta%a =', dta%a
  print *, 'dta%b =', dta%b

  !Testcase 2 : 2 level derived type

  !Nested Structure Constructors
  dtb = dt1(dt0(5,10),5 , 10)
  call csub2(dtb)

  if (dtb%a /= 2) then
     error stop 225

  else if (dtb%b /= 4) then
     error stop 226

  else if (dtb%d0%a /= 2) then
     error stop 227

  else  if  (dtb%d0%b /= 4) then
     error stop 228
  endif

  !Testcase 3 : 3 level derived type

  !Nested Structure Constructors
  dtc = dt2(dt1(dt0(5,10),5,10),5,10)

  call csub3(dtc)

  if (dtc%a /= 2) error stop 229

  if (dtc%b /= 4) error stop 330

  if (dtc%d1%a /= 2) error stop 331

  if  (dtc%d1%b /= 4) error stop 332


  if (dtc%d1%d0%a /= 2) error stop 333

  if  (dtc%d1%d0%b /= 4) error stop 334

end program p1
