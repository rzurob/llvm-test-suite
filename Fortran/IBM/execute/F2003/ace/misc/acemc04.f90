!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acemc04
!*
!*  DATE                       : 2006-07-21
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Interoperability with C: values from C functions (character, logical)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Interoperability, C
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  In AC, invoke C functions returning logicals or characters, and verify
!*  their correctness.  Pay attention to the fact that C and FORTRAN have
!*  different notions of "true" and "false" (C: non-zero vs. zero; FORTRAN:
!*  odd vs. even (at least XLF) - the exact representation seems to be
!*  processor dependent - as long as "true" is encoded as 1 and "false" as 0,
!*  we should be okay).  Also note that FORTRAN's character variables can
!*  have many bytes, whereas C is a single char.  Unicode may also be an
!*  issue (16 bits vs. 8 for ASCII).
!*
!*  The test is successful if the values are identical.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acemc04

  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_arithmetic
  implicit none

  interface

     integer(C_INT) function bool_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function bool_size

     integer(C_INT) function char_size() bind(c)
       use, intrinsic :: iso_c_binding
     end function char_size

     function bool_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       logical(C_BOOL) :: bool_fun
       integer(C_INT), value :: sel
     end function bool_fun

     function char_fun(sel) bind (c)
       use, intrinsic :: iso_c_binding
       character(C_CHAR) :: char_fun
       integer(C_INT), value :: sel
     end function char_fun

  end interface

  logical (C_BOOL) ::   larr(5), larr2(5), larrExp(5)
  character (C_CHAR) :: carr(5), carr2(5), carrExp(5)

  integer(4) :: errorCount

  integer(C_INT) :: i


  larr    = (/ (bool_fun(i),i=0,4) /)
  larr2   = (/ bool_fun(0), bool_fun(1), bool_fun(2), bool_fun(3), bool_fun(4) /)
  larrExp = (/ .false., .true., .not. .false., .not. .true., .false. .neqv. .true. /)

  if (any((larr .neqv. larrExp) .or. (larr2 .neqv. larrExp))) error stop 1_4


  larr    = (/ logical(C_BOOL):: (bool_fun(i),i=0,4) /)
  larr2   = (/ logical(C_BOOL):: bool_fun(0), bool_fun(1), bool_fun(2), bool_fun(3), bool_fun(4) /)
  larrExp = (/ logical(C_BOOL):: .false., .true., .not. .false., .not. .true., .false. .neqv. .true. /)

  if (any((larr .neqv. larrExp) .or. (larr2 .neqv. larrExp))) error stop 2_4


  carr    = (/ (char_fun(i),i=0,4) /)
  carr2   = (/ char_fun(0), char_fun(1), char_fun(2), char_fun(3), char_fun(4) /)
  carrExp = (/ 'A', 'z', char(0), '~', char(255) /)

  if (any((carr .ne. carrExp) .or. (carr2 .ne. carrExp))) error stop 3_4

  carr    = (/ character(C_CHAR):: (char_fun(i),i=0,4) /)
  carr2   = (/ character(C_CHAR):: char_fun(0), char_fun(1), char_fun(2), char_fun(3), char_fun(4) /)
  carrExp = (/ character(C_CHAR):: 'A', 'z', char(0), '~', char(255) /)

  if (any((carr .ne. carrExp) .or. (carr2 .ne. carrExp))) error stop 4_4

end program acemc04
