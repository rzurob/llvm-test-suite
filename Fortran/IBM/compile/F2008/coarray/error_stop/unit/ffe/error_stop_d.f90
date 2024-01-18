!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : error_stop_d.f
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : July 30, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : error stop statement 
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*
!*  DESCRIPTION                :
!*
!*    The stop code must be a scalar constant expression of character or integer type. 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  Program Error_stop_d
  integer, parameter :: a=1
  character, parameter :: c='n'
 
    !good
    error stop
    error stop 0+0
    error stop 1+4
    error stop 4-1+(+a)
    error stop a+a
    error stop int(1.1)
    error stop (a)
    error stop +2
    error stop -2
    error stop +2_1
    error stop +2_8
    
    error stop "werw"
    error stop 'werw'
    error stop ''//""
    error stop 's'//"t"//"op"
    error stop c//"!stop"
  
  end

  subroutine s()
  integer, parameter :: a=1
  character, parameter :: c='n'

    !bad
    error stop aa+aa
    error stop a+0.
    error stop [c]
    !error stop *a
    errorstop [1,1]
    errorstop +2
    errorstop

    do 1 i=1, 10
  1 error stop -1

  END subroutine
 
  pure subroutine s1()
    !bad
    error stop
    error stop "wrong!"
    error stop -11
  end subroutine


