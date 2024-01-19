! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/20/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TYPE parameters,
!*                                1) kind and len attribute
!*                                2) type parameters used in:
!*                                     i) array high bound
!*                                     ii) integer length
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

program dtparamdecl009
  IMPLICIT NONE

  type old(oldkind)
     integer, kind :: oldkind
     integer(oldkind) :: b
  end type

  type old2

     integer(4) :: b
  end type

  type, extends(old):: base(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     integer(basekind) :: basearray(baselen + 1)
     integer(basekind) :: basescalar

  end type

  type, extends(old2):: base2

     integer(4) :: basearray(4 + 1)
     integer(4) :: basescalar

  end type

  type, extends(old2):: base3

     integer(4) :: basearray(4 + 1)
     integer(4) :: basescalar
     type(base2) :: base2M

  end type

  type(base(4,4,:)), allocatable :: b1

  type(base(4, basekind=4,baselen=4)) ::b2
  type(base2) :: b3
  type(base3) :: b4

  print *, ' before allocate'
  allocate(base(4,4,4):: b1)
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

  print *, ' b1%baselen=', b1%baselen
  print *, ' b1%basekind=', b1%basekind
  print *, ' b1%basekind + b1%baselen=', b1%basekind + b1%baselen


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

