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
! %GROUP: dtparamdecl010.f
! %VERIFY: dtparamdecl010.out:dtparamdecl010.vf
! %STDIN:
! %STDOUT: dtparamdecl010.out
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
!*                                     i) array high bound
!*                                     ii) integer length
!*                                     iii) array low bound
!*                                3) no default values for type param
!*                                4) one level of inheritance
!*                                5) kind type parameter values are constants, len are deferred
!*                                6) type parameter values have type parameter keywords
!*                                7) no modules
!*                               10) intrinsics tested - offsetof, sizeof
!*                               11) type parameter inquiry tested
!*                               12) statements tested
!*                                   i) allocate - with type spec
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

program dtparamdecl010
  IMPLICIT NONE
  
  type old(oldkind)
     integer, kind :: oldkind
     integer(oldkind) :: b
  end type
  
  type old2
     
     integer(4) :: b
  end type

  type, extends(old):: base(basekind, baselow, basehigh)
     integer, kind :: basekind
     integer, len :: baselow
     integer, len :: basehigh
     integer(basekind) :: basearray(baselow - 2: 1 + basehigh + baselow)
     integer(basekind) :: basescalar
     
  end type
  
  type, extends(old2):: base2
     
     integer(4) :: basearray(1 + 2 + 3)
     integer(4) :: basescalar
     
  end type
  
  type, extends(old2):: base3
     
     integer(4) :: basearray(1 + 2 + 3)
     integer(4) :: basescalar
     type(base2) :: base2M
     
  end type
  
  type(base(4,4,:,:)), allocatable :: b1
  
  type(base(4, basekind=4,baselow=3, basehigh=2)) ::b2 
  type(base2) :: b3
  type(base3) :: b4
  
  print *, ' before allocate'
  allocate(base(4,4,3,2):: b1)
  print *, ' after allocate'
  b1%basescalar = sizeof(b1)
  b1%basescalar = 15
  b2%basescalar = 15
  b3%basescalar = 15
  
  print *, ' sizeof b1=', sizeof(b1)
  print *, ' sizeof b2=', sizeof(b2)
  print *, ' sizeof b3=', sizeof(b3)
  
  print *, ' offsetof(b3, b3%basescalar)=', offsetof(b3, b3%basescalar) 
  print *, ' offsetof(b2, b2%basescalar)=', offsetof(b2, b2%basescalar) 
  print *, ' offsetof(b1, b1%basescalar)=', offsetof(b1, b1%basescalar) 
  print *, ' offsetof(b1%old, b1%b)=', offsetof(b1%old, b1%b) 
  print *, ' offsetof(b4, b4%base2M%basescalar)=', offsetof(b4, b4%base2M%basescalar)
   print *, ' offsetof(b4%base2M, b4%base2M%basescalar)=', offsetof(b4%base2M, b4%base2M%basescalar)
  b4%base2M%basescalar = 10
  
  print *, ' b1%basescalar=',  b1%basescalar, '  b2%basescalar=',  b2%basescalar 
  
  print *, ' b1%baselow=', b1%baselow
  print *, ' b1%basehigh=', b1%basehigh
  print *, ' b1%basekind=', b1%basekind
  print *, ' b1%basekind + b1%baselow + b1%basehigh=', b1%basekind + b1%baselow + b1%basehigh
  
  
  if ( b1%basescalar .eq.  b2%basescalar ) then
    print *, ' basescalar 1 -ok'
  else
    print *, ' basescalar 1 -not ok'
  end if    
  
  if ( b1%basescalar .eq.  b3%basescalar ) then
    print *, ' basescalar 2 -ok'
  else
    print *, ' basescalar 2 -not ok'
  end if    
  
end  
  
  