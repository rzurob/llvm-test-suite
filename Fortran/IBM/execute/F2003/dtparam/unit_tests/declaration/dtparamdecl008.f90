!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dtparamdecl008.f
! %VERIFY: dtparamdecl008.out:dtparamdecl008.vf
! %STDIN:
! %STDOUT: dtparamdecl008.out
! %EXECARGS:
! %POSTCMD:
! %END
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
!*                                     ii) derived types
!*                                3) no default values for type param
!*                                4) no inheritance
!*                                5) type parameter values are constants
!*                                6) type parameter values have type parameter keywords
!*                                7) type definition in module
!*                                9) components in derived template have
!*                                     i) allocatable
!*                               10) intrinsics tested - lbound, ubound, kind
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
module dtparamdecl008m
  type person1
     integer :: pnumber
     integer :: ptype
  end type

  type old(oldkind, oldlen)
     integer, kind :: oldkind
     integer, len :: oldlen
     integer(oldkind) :: b(oldlen)
  end type

  type old2(oldkind, oldlow, oldhigh )
     integer, kind :: oldkind
     integer, len :: oldlow
     integer, len :: oldhigh
     integer(oldkind) :: b(oldlow:oldhigh)
  end type
end module

module dtparamdecl008m2

use dtparamdecl008m

 type base(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     type(person1) :: person1a(baselen)
     type(person1) :: person1b
     type(old(oldlen=10, oldkind=basekind + 4))  :: old2
     type(old(oldlen=baselen, oldkind=basekind)), allocatable :: old3
     type(old2(oldlow=1, oldhigh=baselen, oldkind=basekind)) :: old2a
     type(old(oldlen=baselen -2, oldkind=basekind))  :: old4
     type(old(oldlen=(2 * baselen), oldkind=basekind))  :: old5
     type(old(oldlen=(baselen / 2), oldkind=basekind))  :: old6
     type(old(oldlen=(-baselen + 8), oldkind=basekind))  :: old7

  end type
end module

program dtparamdecl008
use dtparamdecl008m2



  integer :: i
  type(base(4,4)) :: base1, base1a

  if ((lbound(base1%old4%b, 1) .eq. 1) .and. &
      (ubound(base1%old4%b, 1) .eq. 2)) then
   print *, ' bounds base1%old4%b ok'
  else
   print *, ' bounds base1%old4%b failed'
  end if
  if ((lbound(base1%old5%b, 1) .eq. 1) .and. &
      (ubound(base1%old5%b, 1) .eq. 8)) then
   print *, ' bounds base1%old5%b ok'
  else
   print *, ' bounds base1%old5%b failed ubound(base1%old5%b, 1)=', ubound(base1%old5%b, 1)
  end if
  if ((lbound(base1%old6%b, 1) .eq. 1) .and. &
      (ubound(base1%old6%b, 1) .eq. 2)) then
   print *, ' bounds base1%old6%b ok'
  else
   print *, ' bounds base1%old6%b failed'
  end if
  if ((lbound(base1%old7%b, 1) .eq. 1) .and. &
      (ubound(base1%old7%b, 1) .eq. 4)) then
   print *, ' bounds base1%old7%b ok'
  else
   print *, ' bounds base1%old7%b failed'
  end if


  if ((lbound(base1%person1a, 1) .eq. 1) .and. &
      (ubound(base1%person1a, 1) .eq. 4)) then
   print *, ' bounds base1%person1a ok'
  else
   print *, ' bounds base1%person1a failed'
  end if

  do i=1,4
    base1%person1a(i)%pnumber = 50 + i
    base1%person1a(i)%ptype = i
  end do
  print *, ' base1%person1a=', base1%person1a

  base1%person1b%pnumber = 60
  base1%person1b%ptype = 2
  print *, ' base1%person1b=', base1%person1b

  if ((lbound(base1%old2%b, 1) .eq. 1) .and. &
      (ubound(base1%old2%b, 1) .eq. 10)) then
   print *, ' bounds base1%old2%b ok'
  else
   print *, ' bounds base1%old2%b failed'
  end if

  do i=1,10
    base1%old2%b(i) = 70 + i
  end do
  print *, ' base1%old2%b=', base1%old2%b
  if (kind(base1%old2%b) .eq. 8) then
    print *, ' kind base1%old2%b ok'
  else
    print *, ' kind base1%old2%b failed'
  end if

  allocate(base1%old3)
  if (kind(base1%old3%b) .eq. 4) then
    print *, ' kind base1%old3%b ok'
  else
    print *, ' kind base1%old3%b failed'
  end if

  if ((lbound(base1%old3%b, 1) .eq. 1) .and. &
      (ubound(base1%old3%b, 1) .eq. 4)) then
   print *, ' bounds base1%old3%b ok'
  else
   print *, ' bounds base1%old3%b failed'
  end if

  do i=1,4
    base1%old3%b(i) = 80 + i
  end do
  print *, ' base1%old3%b=', base1%old3%b




  if ((lbound(base1%old2a%b, 1) .eq. 1) .and. &
      (ubound(base1%old2a%b, 1) .eq. 4)) then
   print *, ' bounds base1%old2a%b ok'
  else
   print *, ' bounds base1%old2a%b failed'
  end if

  do i=1,4
    base1%old2a%b(i) = 90 + i
  end do
  print *, ' base1%old2a%b=', base1%old2a%b

 ! ------------------------------------------------
  base1a = base1

  if ((lbound(base1a%person1a, 1) .eq. 1) .and. &
      (ubound(base1a%person1a, 1) .eq. 4)) then
   print *, ' bounds base1a%person1a ok'
  else
   print *, ' bounds base1a%person1a failed'
  end if

  print *, ' base1a%person1a=', base1a%person1a
  print *, ' base1a%person1b=', base1a%person1b

  if ((lbound(base1a%old2%b, 1) .eq. 1) .and. &
      (ubound(base1a%old2%b, 1) .eq. 10)) then
   print *, ' bounds base1a%old2%b ok'
  else
   print *, ' bounds base1a%old2%b failed'
  end if

  print *, ' base1a%old2%b=', base1a%old2%b
  if (kind(base1a%old2%b) .eq. 8) then
    print *, ' kind base1a%old2%b ok'
  else
    print *, ' kind base1a%old2%b failed'
  end if

  if (kind(base1a%old3%b) .eq. 4) then
    print *, ' kind base1a%old3%b ok'
  else
    print *, ' kind base1a%old3%b failed'
  end if

  if ((lbound(base1a%old3%b, 1) .eq. 1) .and. &
      (ubound(base1a%old3%b, 1) .eq. 4)) then
   print *, ' bounds base1a%old3%b ok'
  else
   print *, ' bounds base1a%old3%b failed'
  end if

  print *, ' base1a%old3%b=', base1a%old3%b
end

