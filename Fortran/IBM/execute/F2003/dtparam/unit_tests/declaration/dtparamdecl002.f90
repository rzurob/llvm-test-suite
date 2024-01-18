!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dtparamdecl002.f
! %VERIFY: dtparamdecl002.out:dtparamdecl002.vf
! %STDIN:
! %STDOUT: dtparamdecl002.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Chris Tandy
!*  DATE                       : 09/20/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : TYPE parameters,                                  
!*                                1) kind and len attribute
!*                                2) type parameters used in:
!*                                     i) array low and high bound
!*                                     ii) real length
!*                                3) no default values for type param
!*                                4) no inheritance
!*                                5) type parameter values are constants
!*                                6) type parameter values have type parameter keywords
!*                                7) no modules
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamdecl002
  type, extends(old) :: new
  end type
 
  type old
     integer :: b
  end type

  type base(basekind, low1,  high1)
     integer, kind :: basekind
     integer, len :: low1, high1
     real(basekind) :: basearray(low1:high1)
     real(basekind) :: basearray2(1:high1 - 2)
     
  end type
  
  type(base(4,1,4)) :: base1
  
  type(base(low1=1, basekind=8,high1=10)) :: base2
  
  base1%basearray = 3.0
  base1%basearray(2) = 6.0
  
  base1%basearray2 = 123.0
  base1%basearray2(2) = 678.0
  
  base2%basearray = 5.0
  base2%basearray(2) = 9.0
  
  base2%basearray2 = 345678.0
  base2%basearray2(2) = 987654.0
  
  print *, 'ubound(base1%basearray)=', ubound(base1%basearray)
  print *, 'lbound(base1%basearray)=', lbound(base1%basearray),  ' base1=', base1
  
  print *, 'ubound(base2%basearray)=', ubound(base2%basearray)
  print *, 'ubound(base2%basearray2)=', ubound(base2%basearray2)
  print *, 'lbound(base2%basearray)=', lbound(base2%basearray),  ' base2=', base2
end  
  
  