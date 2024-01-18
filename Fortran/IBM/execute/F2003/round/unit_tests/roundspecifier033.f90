!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier033.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : roundspecifier033
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : Dec. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : functional testing of round edit descriptor in 
!*                               write, WRITE statements with
!*                               es edit descriptor 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  real(4) :: num1, num2, num3, num4
  real(8) :: num5, num6, num7, num8
  real(16) :: num9, num10, num11, num12
  
  character(20) :: round_mode

  namelist /namelist_real/ num1, num2, num3, num4, num5, num6, num7, num8, num9, num10, num11, num12

  num1=6.452768
  num2=6.452762
  num3=-6.452768
  num4=-6.452762
  num5=6.45279031275468
  num6=6.45279031275462
  num7=-6.45279031275468
  num8=-6.45279031275462
  num9=6.4527903127549254938759383268
  num10=6.4527903127549254938759383262
  num11=-6.4527903127549254938759383268
  num12=-6.4527903127549254938759383262

  open(unit=2, file='roundspecifier033.out', round='nearest')

  write(2,nml=namelist_real, round='up')
  write(2,nml=namelist_real, round='down')
  write(2,nml=namelist_real, round='zero')
  write(2,nml=namelist_real, round='nearest')
  write(2,nml=namelist_real, round='compatible')
  write(2,nml=namelist_real, round='processor_defined')
  
  round_mode='up'
  write(2,nml=namelist_real, round=round_mode)
  round_mode='down'
  write(2,nml=namelist_real, round=round_mode)
  round_mode='zero'
  write(2,nml=namelist_real, round=round_mode)
  round_mode='nearest'
  write(2,nml=namelist_real, round=round_mode)
  round_mode='compatible'
  write(2,nml=namelist_real, round=round_mode)
  round_mode='processor_defined'
  write(2,nml=namelist_real, round=round_mode)
  
end
